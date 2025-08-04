#' @md
#' @importFrom ggplot2 autoplot
#' @export
#' @seealso [autoplot.ycevo()]
ggplot2::autoplot


#' Plot the estimated discount functions and yield curves with ggplot2 or plotly
#'
#' Plots the estimated discount functions and yield curves from a [ycevo()] object with `ggplot2` or `plotly`.
#'
#'
#' @md
#' @inheritParams augment.ycevo
#' @param object A [ycevo] object
#' @param est String. Indicates which estimated values to plot: discount function, yield curve, or
#'   both. Default is both.
#' @param against String. Indicates which variable to plot against, i.e. what is on the x axis.
#'   Time-to-maturity `tau`, quotation date `x`, or both (requires package
#'   `plotly`). If both, an interactive 3D plot is generated.
#'
#' @returns A [ggplot2::ggplot()] object if only one dimension is specified in
#'   `against`. A [plotly::plot_ly()] object if `against` is set to both.
#'
#' @seealso [ycevo()]
#' @examples
#' # Simulating bond data
#' bonds <- ycevo_data(n = 10)
#' \donttest{
#' # Estimation can take up to 30 seconds
#' res <- ycevo(bonds, x = lubridate::ymd("2023-03-01"))
#' # Plot
#' autoplot(res)
#' }
#'
#' @importFrom ggplot2 facet_grid
#' @export
autoplot.ycevo <- function(
  object,
  est = c("both", "discount", "yield"),
  against = c("tau", "x", "both"),
  loess = TRUE,
  ...
) {
  qdate_label <- attr(object, "qdate_label")
  est <- match.arg(est)
  # Decide which estimate to drop
  # from which estimate to plot
  which_drop <- switch(
    est,
    both = character(0),
    discount = ".yield",
    yield = ".discount"
  )
  df_plot <- augment(object, loess = loess) %>%
    # Drop estimates not plotted
    select(!all_of(which_drop)) %>%
    # The one(s) not dopped is saved in .est
    tidyr::pivot_longer(
      any_of(c(".discount", ".yield")),
      names_to = ".est",
      values_to = ".value"
    )
  against <- match.arg(against)
  switch(
    against,
    tau = {
      df_plot %>%
        ggplot(aes(
          x = .data$tau,
          y = .data$.value,
          colour = !!sym(qdate_label),
          group = !!sym(qdate_label)
        )) +
        geom_line() +
        facet_grid(".est", scales = "free_y")
    },
    x = {
      if (length(unique(getElement(df_plot, qdate_label))) == 1) {
        warning(
          "Only one quotation date in the estimation output. Line plot is not feasible."
        )
      }
      df_plot %>%
        ggplot(aes(
          x = !!sym(qdate_label),
          y = .data$.value,
          colour = .data$tau,
          group = .data$tau
        )) +
        geom_line() +
        facet_grid(".est", scales = "free_y")
    },
    both = {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        stop(
          "Package \"plotly\" must be installed when \"against = \"both\"\".",
          call. = FALSE
        )
      }
      if (est == "both") {
        stop(
          "\"est\" must be one of \"discount\" or \"yield\" when when \"against = \"both\"\"."
        )
      }
      plotly::plot_ly(
        df_plot,
        x = ~tau,
        y = ~qdate,
        z = ~.value,
        type = 'scatter3d',
        mode = 'lines',
        split = ~qdate
      ) %>%
        plotly::layout(scene = list(zaxis = list(title = est)))
    }
  )
}

#' Plot the estimated discount functions and yield curves
#'
#' Plots the estimated discount functions and yield curves from a [ycevo()] object.
#'
#'
#' @md
#' @inheritParams augment.ycevo
#' @inheritParams autoplot.ycevo
#' @param x A [ycevo] object
#' @param est String. Indicates which estimated values to plot: discount function, yield curve, or
#'   both. Default is both.
#' @param against String. Indicates which variable to plot against, i.e. what is on the x axis.
#'   Time-to-maturity `tau` or quotation date `x`.
#'
#' @returns `NULL`. The functions produces a plot.
#'
#' @seealso [ycevo()], [autoplot.ycevo()]
#' @examples
#' # Simulating bond data
#' bonds <- ycevo_data(n = 10)
#' \donttest{
#' # Estimation can take up to 30 seconds
#' res <- ycevo(bonds, x = lubridate::ymd("2023-03-01"))
#' # Plot
#' plot(res)
#' }
#'
#' @export
plot.ycevo <- function(
  x,
  est = c("both", "discount", "yield"),
  against = c("tau", "x"),
  loess = TRUE,
  ...
) {
  qdate_label <- attr(x, "qdate_label")
  est <- match.arg(est)
  df <- augment(x, loess = loess)
  against <- match.arg(against)

  if (est == "both") {
    oldpar <- par(mfrow = c(2, 1))
    on.exit(par(oldpar))
  }

  col_fun <- scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7")

  if (against == "tau") {
    n_qdate <- length(unique(getElement(df, qdate_label)))
    if (n_qdate == 1) {
      warning(
        "Only one quotation date in the estimation output. Line plot is not feasible."
      )
    }

    cols <- col_fun(seq(0, 1, length.out = n_qdate))
    # plot with tau as the x axis
    df_tau <- split(df, df$qdate)
    if (est %in% c("both", "discount")) {
      # discount
      # par(mar = c(0, 4, 4, 2))
      plot(
        x = df_tau[[1]]$tau,
        y = df_tau[[1]]$.discount,
        type = "l",
        col = cols[[1]],
        ylim = range(df$.discount),
        ylab = "discount",
        xlab = "tau"
      )
      if (length(df_tau) > 1) {
        for (i in seq(2, length(df_tau), by = 1)) {
          lines(x = df_tau[[i]]$tau, y = df_tau[[i]]$.discount, col = cols[[i]])
        }
      }
      legend(
        "topright",
        col = cols[c(1L, n_qdate)],
        lty = 1,
        x.intersp = 0.5,
        y.intersp = 0.5,
        legend = range(df$qdate)
      )
    }
    if (est %in% c("both", "yield")) {
      # yield
      # par(mar = c(5, 4, 0, 2))
      plot(
        x = df_tau[[1]]$tau,
        y = df_tau[[1]]$.yield,
        type = "l",
        col = cols[[1]],
        ylim = range(df$.yield),
        ylab = "yield",
        xlab = "tau"
      )
      if (length(df_tau) > 1) {
        for (i in seq(2, length(df_tau), by = 1)) {
          lines(x = df_tau[[i]]$tau, y = df_tau[[i]]$.yield, col = cols[[i]])
        }
      }
      legend(
        "topright",
        col = cols[c(1L, n_qdate)],
        lty = 1,
        x.intersp = 0.5,
        y.intersp = 0.5,
        legend = range(df$qdate)
      )
    }
  } else if (against == "x") {
    # plot with qdate as the x axis
    n_tau <- length(unique(df$tau))
    cols <- col_fun(seq(0, 1, length.out = n_tau))
    df_qdate <- split(df, df$tau)
    if (est %in% c("both", "discount")) {
      # discount
      plot(
        x = getElement(df_qdate[[1]], qdate_label),
        y = df_qdate[[1]]$.discount,
        type = "l",
        col = cols[[1]],
        ylim = range(df$.discount),
        ylab = "discount",
        xlab = qdate_label
      )
      if (length(df_qdate) > 1) {
        for (i in seq(2, length(df_qdate), by = 1)) {
          lines(
            x = getElement(df_qdate[[i]], qdate_label),
            y = df_qdate[[i]]$.discount,
            col = cols[[i]]
          )
        }
      }
      legend(
        "topright",
        col = cols[c(1L, n_tau)],
        lty = 1,
        x.intersp = 0.5,
        y.intersp = 0.5,
        legend = round(range(df$tau), 2L)
      )
    }
    if (est %in% c("both", "yield")) {
      # yield
      plot(
        x = getElement(df_qdate[[1]], qdate_label),
        y = df_qdate[[1]]$.yield,
        type = "l",
        col = cols[[1]],
        ylim = range(df$.yield),
        ylab = "yield",
        xlab = qdate_label
      )
      if (length(df_qdate) > 1) {
        for (i in seq(2, length(df_qdate), by = 1)) {
          lines(
            x = getElement(df_qdate[[i]], qdate_label),
            y = df_qdate[[i]]$.yield,
            col = cols[[i]]
          )
        }
      }
      legend(
        "topright",
        col = cols[c(1L, n_tau)],
        lty = 1,
        x.intersp = 0.5,
        y.intersp = 0.5,
        legend = round(range(df$tau), 2L)
      )
    }
  }
}
