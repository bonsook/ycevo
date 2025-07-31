prep_windows <- function(
  data,
  xgrid,
  hx,
  rgrid,
  hr,
  tau,
  ht,
  tau_p,
  htp,
  interest,
  units,
  interest_grid
) {
  # kernel weights on x (date, time)
  mat_weights_qdatetime <- get_weights(
    xgrid,
    hx,
    len = length(unique(data$qdate))
  )
  day_idx <- range_idx_nonzero(mat_weights_qdatetime, threshold = 0)
  nday <- length(xgrid)

  # kernel weights on the covariate (interest)
  joint_window <- NULL
  day_grid <- NULL
  if (interest_grid) {
    # kernel weight function in relation to interest rate grids
    r_window <- calc_epaker_weights(interest, rgrid, hr)
    day_grid <- expand.grid(ug = xgrid, rg = rgrid)
    nday <- nrow(day_grid)
    joint_window <- matrix(0, nrow(mat_weights_qdatetime), nday)
    for (j in seq_along(rgrid)) {
      for (i in seq_along(xgrid)) {
        joint_window[, (j - 1) * length(xgrid) + i] <- mat_weights_qdatetime[,
          i
        ] *
          r_window[, j]
      }
    }
    apply(joint_window, 2, function(y) {
      if (all(y == 0)) {
        day_idx <- c(0, 0)
      } else {
        window_idx <- which(y != 0)
        day_idx <- c(window_idx[1], window_idx[length(window_idx)])
      }
      day_idx
    }) %>%
      t() -> day_idx
    obs <- which(day_idx[, 1] != 0)
    day_grid <- day_grid[obs, ]
    joint_window <- joint_window[, obs]
    day_idx <- day_idx[obs, ]
    nday <- length(obs)
    if (nday == 1) {
      joint_window <- matrix(joint_window, ncol = 1)
      day_idx <- matrix(day_idx, nrow = 1)
    }
  }

  # kernel weights on tau (time-to-maturity)
  stopifnot(is.vector(tau))
  mat_weights_tau <- get_weights(
    tau,
    ht,
    len = as.integer(max(data$tupq)),
    units = units
  )
  tupq_idx_tau <- range_idx_nonzero(mat_weights_tau, threshold = 0.01)
  ntupq_tau <- length(tau)

  # tau_p
  stopifnot(is.vector(tau_p))
  mat_weights_tau_p <- get_weights(
    tau_p,
    htp,
    len = as.integer(max(data$tupq)),
    units = units
  )
  tupq_idx_tau_p <- range_idx_nonzero(mat_weights_tau_p, threshold = 0.01)
  ntupq_tau_p <- length(tau_p)

  list(
    # qdatetime
    mat_weights_qdatetime = mat_weights_qdatetime,
    day_idx = day_idx,
    nday = nday,
    # interest
    joint_window = joint_window,
    # tau
    mat_weights_tau = mat_weights_tau,
    tupq_idx_tau = tupq_idx_tau,
    ntupq_tau = ntupq_tau,
    mat_weights_tau_p = mat_weights_tau_p,
    tupq_idx_tau_p = tupq_idx_tau_p,
    ntupq_tau_p = ntupq_tau_p,
    day_grid = day_grid
  )
}


# A component of the \code{estimate_yield} function that calculates \eqn{dbar}
#
# Internal function that estimates the discount function without taking into account
# cross products of coupon payments. Not to be used independently.
# @inheritParams ycevo
# @param xgrid A length T numeric vector; the times at which the discount curve will be estimated.
# @param tau A length m numeric vector, or either a 1 x x or T x x numeric matrix. If a T x x matrix, each row represents the time-to-maturity grid
# that each time-to-maturity in the discount function is compared against. Otherwise the same time-to-maturity grid is repeated for each of the T xgrid values
# bandwidth parameter determining the size of the window that corresponds to each time-to-maturity.
# @param price_slist A list of matrices, generated from [calc_price_slist()].
# @param cf_slist A list of matrices, generated from [calc_cf_slist()].
# @param interest_grid Set to TRUE if a covariate is provided. FALSE otherwise.
# @param windows_ls A list of information on kernel windows produced from the [prep_windows()] function
#
# @return Data frame with the following variables
#
# \describe{
#   \item{ug}{Same as input \code{xgrid}}
#   \item{dbar_numer}{Numerator in dbar. See \code{Source}}
#   \item{dbar_denom}{Denominator in dbar. See \code{Source}}
#   \item{xg}{Same as input \code{tau}}
# }
#
calc_dbar <- function(
  data,
  xgrid,
  tau,
  price_slist,
  cf_slist,
  interest_grid,
  windows_ls
) {
  day_idx <- windows_ls$day_idx
  tupq_idx <- windows_ls$tupq_idx_tau
  mat_weights_tau <- windows_ls$mat_weights_tau
  mat_weights_qdatetime <- windows_ls$mat_weights_qdatetime
  joint_window <- windows_ls$joint_window
  ntupq <- windows_ls$ntupq_tau
  nday <- windows_ls$nday
  day_grid <- windows_ls$day_grid

  if (interest_grid) {
    dbar <- calc_dbar_c(
      ntupq,
      day_idx,
      tupq_idx,
      mat_weights_tau,
      joint_window,
      price_slist,
      cf_slist
    )
    day_grid <- day_grid[rep(1:nday, each = ntupq), ]
    dbar <- tibble(
      ug = day_grid$ug,
      rg = day_grid$rg,
      dbar_numer = dbar[, 1],
      dbar_denom = dbar[, 2]
    )
  } else {
    dbar <- calc_dbar_c(
      ntupq,
      day_idx,
      tupq_idx,
      mat_weights_tau,
      mat_weights_qdatetime,
      price_slist,
      cf_slist
    )
    dbar <- tibble(
      ug = rep(xgrid, rep(ntupq, nday)),
      dbar_numer = dbar[, 1],
      dbar_denom = dbar[, 2]
    )
  }
  dbar$xg <- rep(tau, nday)
  dbar
}

# A component of the \code{estimate_yield} function that calculates \eqn{hhat}
#
# Internal function that calculates coupon payment cross products.
# Not to be used independently.
#
# @inheritParams ycevo
# @inheritParams calc_dbar
# @param data A data frame; bond data to estimate discount curve from. See [ycevo_data()] for an example bond data structure.
# @param xgrid A length T numeric vector; the times at which the discount curve will be estimated.
# @param tau_p (Optional), A length m numeric vector, or either a 1 x m or T x m numeric matrix, matching tau input with m allowed to be different to M.
# If a T x m matrix, each row represents the time-to-maturity grid that each time-to-maturity in the discount function is compared against.
# Otherwise the same time-to-maturity grid is repeated for each of the T xgrid values.
# If m = M, and each entry of tau_p is identical to tau, estimation is performed without interpolaton of the h-hat matrix.
# If the entries are not identical each entry of each row of tau_p must be within the range from the smallest to largest value of the corresponding row of tau,
# and linear interpolation of the h-hat matrix is performed.
# If omitted, tau_p is set equal to tau.
#
# @return Data frame with the following variables
#
# \describe{
#   \item{hhat_numer}{Numerator in H hat. See \code{Source}}
#   \item{ug}{Same as input \code{xgrid}}
#   \item{xg}{Same as input \code{tau}}
#   \item{qg}{Same as input \code{tau_p}}
# }
#
calc_hhat_num <- function(
  data,
  xgrid,
  tau,
  tau_p = tau,
  cf_slist = NULL,
  interest_grid,
  windows_ls
) {
  day_idx <- windows_ls$day_idx
  tupq_idx_tau <- windows_ls$tupq_idx_tau
  tupq_idx_tau_p <- windows_ls$tupq_idx_tau_p
  mat_weights_tau <- windows_ls$mat_weights_tau
  mat_weights_qdatetime <- windows_ls$mat_weights_qdatetime
  mat_weights_tau_p <- windows_ls$mat_weights_tau_p
  joint_window <- windows_ls$joint_window
  ntupq_tau <- windows_ls$ntupq_tau
  ntupq_tau_p <- windows_ls$ntupq_tau_p
  nday <- windows_ls$nday
  day_grid <- windows_ls$day_grid
  same_tau <- isTRUE(all.equal(tau, tau_p))

  if (interest_grid) {
    hhat_mat <- calc_hhat_num_c(
      ntupq_tau,
      ntupq_tau_p,
      day_idx,
      tupq_idx_tau,
      tupq_idx_tau_p,
      mat_weights_tau,
      mat_weights_tau_p,
      joint_window,
      cf_slist,
      same_tau = same_tau
    )
    if (same_tau) {
      hhat_mat <- hhat_mat + `diag<-`(t(hhat_mat), 0)
    }
    day_grid <- day_grid[rep(1:nday, each = ntupq_tau_p * ntupq_tau), ]
    hhat <- tibble(hhat_numer = c(hhat_mat), ug = day_grid$ug, rg = day_grid$rg)
  } else {
    hhat_mat <- calc_hhat_num_c(
      ntupq_tau,
      ntupq_tau_p,
      day_idx,
      tupq_idx_tau,
      tupq_idx_tau_p,
      mat_weights_tau,
      mat_weights_tau_p,
      mat_weights_qdatetime,
      cf_slist,
      same_tau = same_tau
    )
    if (same_tau) {
      hhat_mat <- hhat_mat + `diag<-`(t(hhat_mat), 0)
    }
    hhat <- tibble(
      hhat_numer = c(hhat_mat),
      ug = rep(xgrid, rep(ntupq_tau_p * ntupq_tau, nday))
    )
  }

  hhat$xg <- rep(tau, nday * ntupq_tau_p)
  hhat$qg <- rep(tau_p, rep(ntupq_tau, ntupq_tau_p))

  hhat
}

#' @param xgrid Numeric vector. Values between 0 and 1. Time grids over the
#'   entire time horizon (percentile) of the data at which the discount function is
#'   evaluated.
#' @param rgrid (Optional) Numeric vector. Interest rate grids in percentage
#'   at which the discount function is evaluated, e.g. 4.03 means at interest rate
#'   of 4.03\%.
#' @param hr (Optional) Numeric vector. Bandwidth parameter in percentage
#'   determining the size of the window in the kernel function that corresponds
#'   to each interest rate grid (`rgrid`).
#' @param interest (Optional) Numeric vector. Daily short term interest rates.
#'   The length is the same as the number of quotation dates included in the
#'   data, i.e. one interest rate per day.
#' @param cfp_slist (Internal) Experienced users only. A list of matrices,
#'   generated by the internal function `get_cfp_slist`.
#'
#' @describeIn ycevo Experienced users only. Yield estimation with interest rate
#'   and manually selected bandwidth parameters.
#'   Only length one x and length one hx are supported at a time.
#'   Returns a data frame of the yield and discount rate at each combination of
#'   the provided grids.
#' \describe{
#'   \item{discount}{Estimated discount rate}
#'   \item{xgrid}{Same as input `xgrid`}
#'   \item{tau}{Same as input `tau`}
#'   \item{yield}{Estimated yield}
#' }
#' @export
estimate_yield <- function(
  data,
  xgrid,
  hx,
  tau,
  ht,
  tau_p = tau,
  htp = ht,
  rgrid = NULL,
  hr = NULL,
  interest = NULL,
  cfp_slist = NULL
) {
  units <- 365
  if (min(tau) > min(tau_p) || max(tau) < max(tau_p)) {
    stop('tau_p entries must lie inside tau')
  }

  if (!is.null(rgrid) & !is.null(hr) & !is.null(interest)) {
    interest_grid <- TRUE
  } else {
    interest_grid <- FALSE
  }

  # Check inputs
  assert_class(xgrid, "numeric")
  assert_class(hx, "numeric")
  assert_length(xgrid)
  assert_length(hx)
  assert_class(data, "data.frame")
  assert_class(tau, "numeric")
  assert_class(ht, "numeric")
  assert_class(tau_p, "numeric")
  assert_class(htp, "numeric")
  assert_vector(tau)
  assert_vector(ht)
  assert_vector(tau_p)
  assert_vector(htp)
  assert_same_length(xgrid, hx)
  assert_same_length(tau, ht)
  assert_same_length(tau_p, htp)
  if (!all(c("qdate", "id", "price", "pdint", "tupq") %in% colnames(data))) {
    stop(
      "The data object should contain columns qdate, id, price, pdint, and tupq."
    )
  }

  if (is.null(cfp_slist)) {
    cfp_slist <- get_cfp_slist(data)
  }
  cf_slist <- cfp_slist$cf_slist
  price_slist <- cfp_slist$price_slist

  windows_ls <- prep_windows(
    data = data,
    xgrid = xgrid,
    hx = hx,
    rgrid = rgrid,
    hr = hr,
    tau = tau,
    ht = ht,
    tau_p = tau_p,
    htp = htp,
    interest = interest,
    units = units,
    interest_grid = interest_grid
  )
  # Estimate dbar & the numerator of the h-hat matrix
  dbar <- calc_dbar(
    data = data,
    xgrid = xgrid,
    tau = tau,
    price_slist = price_slist,
    cf_slist = cf_slist,
    interest_grid = interest_grid,
    windows_ls = windows_ls
  )

  hhat_num <- calc_hhat_num(
    data = data,
    xgrid = xgrid,
    tau = tau,
    tau_p = tau_p,
    cf_slist = cf_slist,
    interest_grid = interest_grid,
    windows_ls = windows_ls
  )

  # Drop badly specified tau values
  # and do again
  if (any(dbar$dbar_denom == 0)) {
    problem_tau <- filter(dbar, .data$dbar_denom == 0)$xg
    if (!identical(tau_p, tau)) {
      if (!(max(tau_p) <= min(problem_tau) || min(tau_p) >= max(problem_tau))) {
        stop(
          "tau values at ",
          paste(problem_tau, collapse = ", "),
          " does not have enough obs to estimate yield. ",
          "Modified tau and tau_p."
        )
      }
    }
    warning(
      "tau values at ",
      paste(problem_tau, collapse = ", "),
      " does not have enough obs to estimate yield"
    )
    output <- estimate_yield(
      data = data,
      xgrid = xgrid,
      hx = hx,
      tau = tau[!tau %in% problem_tau],
      ht = ht[!tau %in% problem_tau],
      tau_p = tau_p[!tau_p %in% problem_tau],
      htp = htp[!tau_p %in% problem_tau],
      rgrid = rgrid,
      hr = hr
    )
    return(output)
  }

  # The denominator of h-hat entries are estimated as part of dbar
  dbar <- mutate(dbar, dbar = .data$dbar_numer / .data$dbar_denom)
  if (any(is.na(dbar$dbar))) {
    stop("Missing value in dbar")
  }

  hhat <- dbar %>%
    select(any_of(c("ug", "xg", "rg", "dbar_denom"))) %>%
    dplyr::right_join(
      hhat_num,
      by = intersect(c("ug", "rg", "xg"), colnames(dbar))
    ) %>%
    mutate(hhat = .data$hhat_numer / .data$dbar_denom)

  # Create a matrix of interpolation weights
  interpol_weights <- matrix(0, nrow = length(tau), ncol = length(tau_p))

  # Iterate over the values of tau_p
  for (j in 1:length(tau_p)) {
    # If tau_p is contained in tau then the weight will be one
    if (any(tau == tau_p[j])) {
      interpol_weights[which(tau == tau_p[j]), j] <- 1
    } else {
      # Otherwise find the tau immediately above and below this tau
      lower <- max(which(tau < tau_p[j]))
      upper <- min(which(tau > tau_p[j]))
      # Find interpolation weights as ratio between the two tau values lying above and below this tau_p
      dist <- tau[upper] - tau[lower]
      interpol_weights[lower, j] <- (tau_p[j] - tau[lower]) / dist
      interpol_weights[upper, j] <- (tau[upper] - tau_p[j]) / dist
    }
  }

  db <- dbar %>%
    select(any_of(c("ug", "rg", "dbar", "xg"))) %>%
    group_by(across(any_of(c("ug", "rg")))) %>%
    tidyr::nest(.key = "db") %>%
    ungroup()

  hh <- hhat %>%
    select(any_of(c("ug", "rg", "hhat", "qg", "xg"))) %>%
    group_by(across(any_of(c("ug", "rg")))) %>%
    tidyr::nest(.key = "hh") %>%
    ungroup() %>%
    mutate(
      hh = lapply(hh, function(x) {
        x %>%
          arrange(.data$qg, .data$xg) %>%
          tidyr::pivot_wider(names_from = "qg", values_from = "hhat") %>%
          select(-"xg") %>%
          as.matrix() %>%
          unname()
      })
    )

  dhat <- left_join(db, hh, by = intersect(c("ug", "rg"), colnames(db))) %>%
    mutate(
      discount = mapply(
        function(hh, db) {
          # Construct the length(tau) x length(tau) matrix of the interpolated hhat
          hh_interpol <- hh %*% t(interpol_weights)
          X <- diag(1, length(tau)) + hh_interpol
          mutate(db, discount = as.vector(solve(X) %*% dbar), .keep = "unused")
        },
        hh = hh,
        db = db,
        SIMPLIFY = FALSE
      )
    ) %>%
    select(any_of(c("ug", "rg", "discount"))) %>%
    unnest("discount")

  dhat <- dhat %>%
    mutate(yield = discount2yield(.data$discount, .data$xg)) %>%
    dplyr::rename(xgrid = "ug", tau = "xg") %>%
    rename_with(function(x) rep("rgrid", length(x)), any_of("rg"))

  dhat
}

discount2yield <- function(discount, tau) -log(discount) / tau
