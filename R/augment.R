#' @md
#' @importFrom generics augment
#' @export
#' @seealso [augment.ycevo()]
generics::augment


#' Augment data with predicted discount functions and yield curves
#'
#'
#' @details
#' If `newdata` is not provided, returns the discount function and yield curve
#' at the specified estimation points in [ycevo()].
#'
#' If `newdata` is provided, the discount functions at the time-to-maturities
#' specified in `newdata` are generated from loess smoothing (see
#' [stats::loess()]), and interpolated to produce the discount function values at the
#' quotation date specified in `newdata`, before being converted to the yield curves.
#'
#' @md
#' @param x A [ycevo] object
#' @param newdata A data frame containing time-to-maturity in years `tau` and
#'   the quotation date at which the discount functions and the yield curves are to
#'   be predicted. The quotation date is to be named after the quotation
#'   date column in `x`, which is also the name of the quotation date column of
#'   the `data` argument in [ycevo()] that produces `x`. The default is `qdate`.
#' @param loess Logical. If TRUE, the returned discount functions and yield curves
#'   are loess smoothed.
#' @param ... Additional arguments required for generic consistency. Currently
#'   not used. Warning: A misspelled argument will not raise an error. The
#'   misspelled argument will be either disregarded, or the default value will
#'   be applied if one exists.
#'
#' @returns `newdata` augmented with `.discount` and `.yield` for the discount
#'   function and the yield curve respectively.
#' @seealso [ycevo()], [predict.ycevo()]
#' @examples
#' # Simulating bond data
#' bonds <- ycevo_data(n = 10)
#' \donttest{
#' # Estimation can take up to 30 seconds
#' res <- ycevo(bonds, x = lubridate::ymd("2023-03-01"))
#' # Augmentation
#' augment(res)
#' }
#'
#' @export
augment.ycevo <- function(
  x,
  newdata = NULL,
  loess = TRUE,
  ...
) {
  df_flat <- unnest(x, ".est")

  cols <- attr(x, "cols")
  qdate_label <- "qdate"
  if (!is.null(cols)) {
    qdate_label <- vapply(as.list(cols)[-1], as.character, character(1))[
      qdate_label
    ]
  }

  # if newdata isn't provided
  # generate fitted value at the
  # time and time-to-maturity where the curves are estimated
  if (is.null(newdata)) {
    newdata <- df_flat
  }

  if (!loess) {
    norow <- dplyr::anti_join(newdata, df_flat, by = c("tau", qdate_label))
    if (nrow(norow) > 0) {
      stop(
        "If loess = FALSE, newdata should be a subset of the xgrid and tau used to fit ycevo (e.g. the default)."
      )
    }
    return(df_flat)
  }

  if (
    any(newdata$tau > max(df_flat$tau)) || any(newdata$tau < min(df_flat$tau))
  ) {
    warning(
      "tau in newdata outside of the original range of tau used to fit the model are extrapolated from loess."
    )
  }
  newdata <- select(newdata, !any_of(c(".discount", ".yield")))
  interpolate(x, newdata, qdate_label)
}


interpolate <- function(object, newdata, qdate_label) {
  stopifnot(inherits(object, "ycevo"))

  # the names of covariates apart from time
  xnames <- setdiff(colnames(object), c(".est", qdate_label))
  # all names including time
  ax <- c(qdate_label, xnames)
  # all names with "_near." suffix
  near. <- "_near."
  ax. <- paste0(ax, near.)

  # Check new data has all the covariates
  ncl <- xnames[!xnames %in% colnames(newdata)]
  if (length(ncl) > 0) {
    stop(paste(ncl, collapse = ", "), " not found in newdata")
  }

  object <- arrange(object, across(all_of(xnames)))

  # Fit a loess for every group
  df_loess <- object %>%
    mutate(
      loess = lapply(.data$.est, function(data) {
        stats::loess(
          .discount ~ tau,
          # interpolate on the log of discount to avoid negative values
          data = mutate(data, .discount = log(.data$.discount)),
          control = stats::loess.control(surface = "direct")
        )
      })
    ) %>%
    select(!".est")

  # list of unique values of covariates (including time)
  ls_x <- object %>%
    select(all_of(ax)) %>%
    lapply(unique)

  # a function that finds the closest values in a covariate of a group
  # if the value in the newdata matches a value in the original estimation,
  # or the new value is outside of the boundary,
  # return only that one value or one boundary value
  find_near <- function(x) {
    # example
    # x <- seq(ymd("2023-02-01"), ymd("2023-07-01"), by = "1 month")
    target <- getElement(ls_x, dplyr::cur_column())
    l <- length(target)

    int <- match(x, target)

    nomatch <- is.na(int)
    int_nomatch <- findInterval(x[nomatch], target)

    int <- as.list(int)
    int[nomatch] <- lapply(int_nomatch, function(i) {
      unique(pmin(pmax(c(i, i + 1), 1), l))
    })
    lapply(int, function(i) target[i])
  }

  # Find the nearest groups of loess
  df_near <- newdata %>%
    select(all_of(c(qdate_label, xnames, "tau"))) %>%
    distinct() %>%
    mutate(across(
      all_of(c(qdate_label, xnames)),
      find_near,
      .names = paste0("{.col}", near.)
    ))

  if (length(ax) > 1) {
    df_near <- df_near %>%
      # expand grid so every combination of covariates are covered
      mutate(
        near. = mapply(
          function(...) {
            expand.grid(list(...)) %>%
              `colnames<-`(ax.)
          },
          !!!syms(ax.),
          SIMPLIFY = FALSE
        )
      ) %>%
      select(!all_of(ax.)) %>%
      unnest("near.")
  } else {
    df_near <- unnest(df_near, all_of(ax.))
  }

  df_predict <- df_near %>%
    # nest by loess to speed up prediction
    tidyr::nest(.by = ends_with(near.)) %>%
    # rename back to match loess name
    rename_with(function(x) gsub(near., "", x), ends_with(near.)) %>%
    # match loess
    left_join(df_loess, by = c(qdate_label, xnames)) %>%
    # predict discount rate
    mutate(
      .discount = mapply(
        function(data, loess) {
          stats::predict(loess, data$tau)
        },
        data = .data$data,
        loess = .data$loess,
        SIMPLIFY = FALSE
      )
    ) %>%
    # drop loess
    select(!"loess") %>%
    # rename to separate
    rename_with(
      function(x) paste0(x, near.),
      .cols = all_of(c(qdate_label, xnames))
    ) %>%
    # unnest
    unnest(c("data", ".discount"), names_repair = "minimal")

  df_temp <- df_predict %>%
    group_by(!!!syms(c(ax, "tau"))) %>%
    dplyr::summarise(
      .discount = interp(
        list(!!!syms(ax.)),
        .data$.discount,
        lapply(list(!!!syms(ax)), unique)
      ),
      .groups = "drop"
    ) %>%
    # the interpolation was done on the log of discount
    # to prevent negative values
    mutate(.discount = exp(.data$.discount)) %>%
    mutate(.yield = discount2yield(.data$.discount, .data$tau))
  left_join(newdata, df_temp, by = c(qdate_label, xnames, "tau"))
}

# interpolate
# x and xout can be a list of multiple xs to interpolate
interp <- function(x, y, xout) {
  switch(
    as.character(length(x)),
    "1" = interp1(x, y, xout),
    "2" = interp2(x, y, xout),
    stop("Currently only supports one extra predictor (interest rate)")
  )
}
interp1 <- function(x, y, xout) {
  if (length(y) == 1) {
    return(y)
  }
  stats::approx(x = x[[1L]], y = y, xout = xout[[1]], rule = 2)$y
}
interp2 <- function(x, y, xout) {
  # if there is just one y, return y
  if (length(y) == 1) {
    return(y)
  }

  # check if the number of elements match in x and y
  lx <- unique(vapply(x, length, FUN.VALUE = integer(1L)))
  stopifnot(length(lx) == 1)
  stopifnot(lx == length(y))

  # number of covariates
  ld <- length(x)
  # unique value of covariates
  ux <- lapply(x, unique)
  # number of unique value of each covariate
  lux <- vapply(ux, length, FUN.VALUE = integer(1L))

  # construct an array for all the values
  # each dimension is one covariate
  # the values in the array are values of y corresponding to the covariates
  g <- array(dim = lux, dimnames = ux)
  idx <- do.call(cbind, x)
  idx[] <- as.character(idx)
  g[idx] <- y

  # index of dimensions in reverse order
  # interpolate by the largest dimension and work inwards
  # only tested against two covariates
  ds <- seq(ld, 1L, by = -1L)
  for (d in ds) {
    if (is.vector(g)) {
      g <- t(g)
    }
    # skip when there is only one value for that dimension
    if (dim(g)[[ds[[d]]]] == 1) {
      next
    }
    g <- apply(g, d, function(y) {
      stats::approx(
        x = ux[[ds[[d]]]],
        y = y,
        xout = xout[[ds[[d]]]],
        rule = 2
      )$y
    })
  }
  unname(as.vector(g))
}


#' Return predicted discount functions or yield curves
#'
#'
#' @details
#' If `newdata` is not provided, returns the discount function or yield curve
#' at the specified estimation points in [ycevo()].
#'
#' If `newdata` is provided, the discount functions at the time-to-maturities
#' specified in `newdata` are generated from loess smoothing (see
#' [stats::loess()]), and interpolated to produce the discount function values at the
#' quotation date specified in `newdata`, before being converted to the yield curves.
#' @md
#' @param object A [ycevo] object
#' @inheritParams augment.ycevo
#' @param type Whether to return the discount function or the yield curve.
#'
#' @returns A numeric vector of the discount function or the yield curve based on
#' `type`.
#' @seealso [ycevo()], [augment.ycevo()]
#' @examples
#' # Simulating bond data
#' bonds <- ycevo_data(n = 10)
#' \donttest{
#' # Estimation can take up to 30 seconds
#' res <- ycevo(bonds, x = lubridate::ymd("2023-03-01"))
#' # Prediction
#' predict(res)
#' }
#'
#' @export
predict.ycevo <- function(
  object,
  newdata = NULL,
  type = c("discount", "yield"),
  loess = TRUE,
  ...
) {
  type <- match.arg(type)
  getElement(
    augment(object, newdata = newdata, loess = loess, ...),
    paste0(".", type)
  )
}
