
# @title Calculates number of bonds that mature in each tau
# @param data A data frame; bond data to estimate discount curve from.
# @param xgrid A single value for xgrid between 0 and 1
# @param hx A single value for the bandwidth of the xgrid value
# @param tau A numeric vector of the time-to-maturity grid
# for the discount function at the corresponding time.
# @param ht A numeric vector matching tau, bandwidth parameter determining the size of the window
# that corresponds to each time-to-maturity.
# @param rgrid Optional, a single value for rgrid
# @param hr Optional, A single value for the bandwidth of the rgrid value for use with rgrid
# @param interest Optional, A vector of daily interest rates for use with rgrid
# @keywords internal
# @author Bonsoo Koo, Kai-Yang Goh and Nathaniel Tomasetti
#
num_points_mat <- function(data, xgrid, hx, tau, ht, rgrid = NULL, hr = NULL, interest = NULL, units = 365) {
  # Check dates in data matches interest rate
  dates <- unique(data$qdate)
  if(!is.null(interest)){
    if(length(interest) != length(dates)){
      stop('Length of interest rate vector does not match number of unique qdates')
    }
  }
  # Calculate the u and r windows (if r is provided)

  window <- get_weights(xgrid, hx, len = length(unique(data$qdate)))
  if(!is.null(rgrid) & !is.null(hr) & !is.null(interest)){
    windowR <- calc_r_window(interest, rgrid, hr)
    window <- window * windowR
  }

  # Find subset of data with positive kernel
  kernel <- data.frame(qdate = dates, k = window)
  if("k" %in% colnames(data)) colnames(data)[colnames(data) == "k"] <- "original_k"
  data_sub <- data %>%
    left_join(kernel, by = 'qdate') %>%
    filter(.data$k > 0)

  # Calculate number of maturing bonds in each x window
  ### Highly rely on the notion that tupq and tau are in days
  mat_weights_tau <- get_weights(tau, ht,
                                 len = as.integer(max(data$tupq)),
                                 units = units)
  x_idx <- range_idx_nonzero(mat_weights_tau, threshold = 0.01)
  sapply(seq_along(tau),
         function(j) sum(
           dplyr::between(
             as.numeric(data_sub$tupq),
             x_idx[j,1],
             x_idx[j,2]) &
             data_sub$pdint >= 100)
  )
}

#' Generate a yield curve with cubic time evolution
#'
#' Generate a yield curve using the extended version of Nelson & Siegel model
#' (Nelson, C. R., & Siegel, A. F., 1987). This has been used in the simulation
#' setting (Equation (30)) of Koo, B., La Vecchia, D., & Linton, O. (2021). See
#' `Details` and `References`.
#'
#' The initial curve at time zero is generated from the following equation
#' \deqn{Yield_{i, 0} = b_0 + b_1 * ((1 - \exp(-\tau_i / t_1)) / (\tau_i / t_1)) +
#' b_2 * ((1 - \exp(-\tau_i / t_2)) / (\tau_i / t_2) - \exp(-\tau_i / t_2))}
#' where \eqn{\tau_i} is the time to maturity, usually measured in years. This
#' defines the yield curve for the quotation date = 0. The yield curve for
#' quotation dates `time` is obtained by multiplying this curve
#' by the cubic equation,
#' \deqn{Yield_{i, time} = Yield_{i, 0} * (1 + linear * time + quadratic *
#' time^2 + cubic * time^3)}
#' so the yield curve slowly changes over different quotation dates.
#'
#' @md
#' @param n_qdate Integer. Number of quotation dates to use in the
#'   data. Defaults to 12.
#' @param periods Integer. Maximum number of time-to-maturity periods
#'   in 10 years that the yield curve is estimated for each quotation date.
#'   Defaults to 36
#' @param b0 Level term in yield curve equation, Defaults is 0. See
#'   `Details`.
#' @param b1 Slope term in yield curve equation, Defaults is 0.05. See
#'   `Details`.
#' @param b2 Curvature term in yield curve equation, Defaults is 2. See
#'   `Details`.
#' @param t1 Scaling parameter in yield curve equation, Defaults is 0.75. See
#'   `Details`.
#' @param t2 Scaling parameter in yield curve equation, Defaults is 125. See
#'   `Details`.
#' @param linear Linear term in yield curve evolution, Defaults is -0.55. See
#'   `Details`.
#' @param quadratic Quadratic term in yield curve evolution. Defaults is 0.55.
#'   See `Details`.
#' @param cubic Cubic term in yield curve evolution. Defaults is -0.55. See
#'   `Details`.
#'
#' @returns
#' \describe{
#'   \item{`generate_yield()`}{
#'   Numeric matrix. Each column contains the yield curve values at a point in time (a quotation date).
#'   Each row contains the yield curve values for a time-to-maturity.
#'   For example, the number in the second column third row is the yield at the second quotation date,
#'   for the third time-to-maturity ranking from shortest to longest.
#'   See `Details` for the equation to generate a yield curve.
#'   See `Examples` for a example with the code to visually inspect the yield curves.}
#' }
#'
#' @seealso [ycevo_data()]
#' @examples
#' out <- generate_yield()
#'
#' # plots
#' library(ggplot2)
#' out <- data.frame(out)
#' colnames(out) <- 1:12
#' out <- dplyr::mutate(out, time = 1:36)
#' out <- tidyr::pivot_longer(out, -time, names_to = "qdate", values_to = "yield")
#' ggplot(out) +
#'   geom_line(aes(x=time, y=yield, color = qdate))
#'
#' @references Nelson, C. R., & Siegel, A. F. (1987). Parsimonious Modeling of
#'   Yield Curves. The Journal of Business, 60(4), 473-489.
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a
#'   nonparametric model for bond prices from cross-section and time series
#'   information. Journal of Econometrics, 220(2), 562-588.
#' @export
generate_yield <- function(n_qdate = 12, periods = 36,
                           b0 = 0, b1 = 0.05, b2 = 2,
                           t1 = 0.75, t2 = 125,
                           linear = -0.55, quadratic = 0.55, cubic = -0.55){

  tauSeq <- (1:periods)/(periods/10)

  yieldInit <- b0 + b1 * ((1 - exp(- tauSeq / t1)) / ( tauSeq / t1)) +
    b2 * ((1 - exp(- tauSeq / t2)) / (tauSeq / t2) - exp(- tauSeq / t2))

  yield <- matrix(0, periods, n_qdate)
  for(i in 1:n_qdate){
    t <- i / n_qdate
    yield[,i] <- yieldInit * (1 + cubic * t^3 + quadratic * t^2 + linear * t)
  }
  yield


}

# Nelson and Siegel yield model
nelson_siegel <- function(maturity,
                          b0 = 0, b1 = 0.05, b2 = 2,
                          t1 = 0.75, t2 = 125) {
  b0 + b1 * ((1 - exp(- maturity / t1)) / ( maturity / t1)) +
    b2 * ((1 - exp(- maturity / t2)) / (maturity / t2) - exp(- maturity / t2))
}


#' @md
#' @param time Numeric value.
#' @param maturity Numeric value. Maturity in years.
#' @describeIn generate_yield Return the yield at specific points in time of
#'   specific maturities.
#' @returns
#' \describe{
#'   \item{`get_yield_at()`}{Numeric vector.}
#' }
#' @export
get_yield_at <- function(time, maturity,
                         b0 = 0, b1 = 0.05, b2 = 2,
                         t1 = 0.75, t2 = 125,
                         linear = -0.55, quadratic = 0.55, cubic = -0.55) {
  assert_same_length(time, maturity)
  time <- as.numeric(time)
  yieldInit <- nelson_siegel(maturity, b0, b1, b2, t1, t2)
  yieldInit * (1 + cubic * time^3 + quadratic * time^2 + linear * time)

}

#' @md
#' @returns
#' \describe{
#'   \item{`get_yield_at_vec()`}{Numeric vector.}
#' }
#' @describeIn generate_yield Deprecated. Vectorised version of
#'   `get_yield_at()`.
#' @export
get_yield_at_vec <- function(time, maturity,
                             b0 = 0, b1 = 0.05, b2 = 2,
                             t1 = 0.75, t2 = 125,
                             linear = -0.55, quadratic = 0.55, cubic = -0.55) {
  .Deprecated("get_yield_at", "ycevo")
  get_yield_at(time, maturity, b0, b1, b2, t1, t2, linear, quadratic, cubic)
}


# span_x to hx
span2h <- function(span, len, units = len) {
  gamma <- seq_len(len) /units

  obj_fun <- function(h) sum(calc_epaker_weights(gamma, stats::median(gamma), h) > 0)/2 - span
  stats::uniroot(obj_fun, interval = c(0.0001, 1), extendInt = "up")$root
}

# handle user sepecified cols augment in, e.h., ycevo function
# @inheritParams ycevo
# @param cols expression of cols in ycevo
# @param covariate character vector of the column names of the addtional covariate.
# Currently only support one
#' @importFrom rlang enexpr
handle_cols <- function(data, cols, d_col, qdate_label) {

  # variables required but missing in data
  setd <- vars_missing_data <- setdiff(d_col, colnames(data))
  loc <- NULL
  if(!is.null(cols)) {
    loc <- tidyselect::eval_rename(cols, data)
    # variables required but missing in data and not specified by user
    setd <- setdiff(vars_missing_data, names(loc))
    if(any(names(loc) == qdate_label))
      qdate_label <-  colnames(data)[[loc[[qdate_label]]]]
    data <- dplyr::rename(data, !!cols)
  }
  if(length(setd) > 0L) {
    stop(
      "The following columns are required but are not found in the data and are not specified in the cols argument: ",
      paste0(setd, collapse = ", "), call. = FALSE)
  }

  list(data = data, qdate_label = qdate_label, loc = loc)
}

# @inheritParams ycevo
handle_covariates <- function(data, ...) {
  dots <- list(...)

  if(any(temp <- (!names(dots) %in% colnames(data)))){
    stop(paste0(names(dots)[temp], collapse = ", "), " column(s) not found in the data")
  }

  interest <- NULL
  rgrid <- NULL
  hr <- NULL
  if(length(dots) > 0){
    if(length(dots) != 1)
      stop("Currently only supports one extra predictor (interest rate)")
    interest <- data %>%
      select(all_of(c("qdate", names(dots)))) %>%
      arrange(.data$qdate) %>%
      distinct(.data$qdate, .keep_all = TRUE) %>%
      dplyr::pull(names(dots))
    rgrid <- dots[[1]][[1]]
    hr <- dots[[1]][[2]]
  }

  list(interest = interest,
       rgrid = rgrid,
       hr = hr,
       dots_name = names(dots))
}



