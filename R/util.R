
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
#' Generate a yield curve using the extended version of Nelson & Siegel model (Nelson, C. R., & Siegel, A. F., 1987). 
#' This has been used in the simulation setting (Equation (30)) of Koo, B., La Vecchia, D., & Linton, O. (2021).
#' See \code{Details} and \code{References}.
#' 
#' Returns a matrix where each column corresponds to a yield curve at a different point in time.
#' The initial curve at time to maturity zero is estimated from the following equation
#' \deqn{Yield_{i, 0} = b_0 + b_1 * ((1 - \exp(-\tau_i / t_1)) / (\tau / t_1)) + b_2 * ((1 - \exp(-\tau_i / t_2)) / (\tau_i / t_2) - \exp(-\tau_i / t_2))}
#' where \eqn{\tau_i} is the time to maturity, usually measured in years. This defines the yield curve for the quotation date = 0.
#' The yield curve for quotation dates = 1, 2, ... , max_q_date multiplies this curve by the cubic equation,
#' \deqn{Yield_{i, t} = Yield_{i, 0} * (1 + linear * t + quadratic * t^2 + cubic * t^3)}
#' so the yield curve slowly changes over different quotation dates.
#' 
#' @param n_qdate Integer giving the number of quotation dates to use in the data. Defaults to 12.
#' @param periods Integer giving the maximum number of time-to-maturity periods in 10 years that the yield curve is estimated for each quotation date. Defaults to 36
#' @param b0 Level term in yield curve equation, Defaults to 0. See \code{Details}.
#' @param b1 Slope term in yield curve equation, Defaults to 0.05. See \code{Details}.
#' @param b2 Curvature term in yield curve equation, Defaults to 2. See \code{Details}.
#' @param t1 Scaling parameter in yield curve equation, Defaults to 0.75. See \code{Details}.
#' @param t2 Scaling parameter in yield curve equation, Defaults to 125. See \code{Details}.
#' @param linear Linear term in yield curve evolution, Defaults to -0.55. See \code{Details}.
#' @param quadratic Quadratic term in yield curve evolution. Defaults to 0.55. See \code{Details}.
#' @param cubic Cubic term in yield curve evolution. Defaults to -0.55. See \code{Details}.
#' 
#' @return 
#' \describe{
#'   \item{\code{generate_yield}}{
#'   Numeric matrix. Each column is a yield curve in a point in time (a quotation date). Each row is for a time-to-maturity.
#'   For example, the number in the second column third row is the yield for the yield curve at the second quotation date,
#'   for the third time-to-maturity ranking from shortest to longest. See \code{Details} for the equation to generate the yield curve.
#'   See \code{Examples} for a example with the code to visually inspect the yield curves.}
#' }
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
#' @references Nelson, C. R., & Siegel, A. F. (1987). Parsimonious Modeling of Yield Curves. The Journal of Business, 60(4), 473-489.
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a nonparametric model for bond prices from cross-section and time series information. Journal of Econometrics, 220(2), 562-588.
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


#' @param time Numeric value between 0 and 1.
#' @param maturity Numeric value. Maturity in years.
#' @describeIn generate_yield Return the yield at a specific point in time of a specific maturity.
#' @return 
#' \describe{
#'   \item{\code{get_yield_at}}{Numeric scalar.}
#' }
#' @export
get_yield_at <- function(time, maturity, 
                         b0 = 0, b1 = 0.05, b2 = 2, 
                         t1 = 0.75, t2 = 125,
                         linear = -0.55, quadratic = 0.55, cubic = -0.55) {
  
  yieldInit <- b0 + b1 * ((1 - exp(- maturity / t1)) / ( maturity / t1)) + 
    b2 * ((1 - exp(- maturity / t2)) / (maturity / t2) - exp(- maturity / t2))
  yieldInit * (1 + cubic * time^3 + quadratic * time^2 + linear * time)
  
}

#' @param time Numeric value.
#' @param maturity Numeric value. Maturity in years.
#' @return 
#' \describe{
#'   \item{\code{get_yield_at_vec}}{Numeric vector.}
#' }
#' @describeIn generate_yield Vectorised version of \code{get_yield_at}.
#' @export
get_yield_at_vec <- function(time, maturity, 
                             b0 = 0, b1 = 0.05, b2 = 2, 
                             t1 = 0.75, t2 = 125,
                             linear = -0.55, quadratic = 0.55, cubic = -0.55) {
  mapply(get_yield_at, time, maturity, 
         b0 = b0, b1 = b1, b2 = b2, 
         t1 = t1, t2 = t2,
         linear = linear, quadratic = quadratic, cubic = cubic)
}
