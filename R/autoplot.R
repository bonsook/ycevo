#' @importFrom ggplot2 autoplot facet_grid
#' @export
autoplot.ycevo <- function(
    object, est = c("both", "discount", "yield"),
    against = c("tau", "x", "both"), loess = TRUE, ...) {
  qdate_label <- attr(object, "qdate_label")
  est <- match.arg(est)
  which_drop <- switch(
    est, 
    both = character(0), 
    discount = ".yield",
    yield = ".discount"
  )
  df_plot <- augment(object, loess = loess) %>% 
    mutate(!!sym(qdate_label)) %>% 
    select(!all_of(which_drop)) %>% 
    tidyr::pivot_longer(any_of(c(".discount", ".yield")), 
                 names_to = ".est", 
                 values_to = ".value")
  against <- match.arg(against)
  switch (
    against,
    tau = {
      df_plot %>%
        ggplot(aes(x=.data$tau, y=.data$.value, colour = !!sym(qdate_label),
                   group = !!sym(qdate_label))) +
        geom_line() +
        facet_grid(".est", scales = "free_y")
    },
    x = {
      df_plot %>%
        ggplot(aes(x=!!sym(qdate_label), y=.data$.value, colour = .data$tau,
                   group = .data$tau)) +
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
      if(est == "both") 
        stop("\"est\" must be one of \"discount\" or \"yield\" when when \"against = \"both\"\".")
      plotly::plot_ly(df_plot, 
                      x = ~tau, y = ~qdate, z = ~.value, 
                      type = 'scatter3d', mode = 'lines', 
                      split = ~qdate) %>%
        plotly::layout(scene = list(zaxis = list(title = est)))
    }
  )
}

