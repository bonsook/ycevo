# @title Calculates the number of payments in each qgrid
# @param .data A data frame; bond data to estimate discount curve from.
# @param ugrid A single value for ugrid between 0 and 1
# @param hu A single value for the bandwidth of the ugrid value
# @param qgrid A numeric matrix, each row represents the time-to-maturity grid
# for the discount function at the corresponding time.
# @param hq A numeric vector, bandwidth parameter determining the size of the window
# that corresponds to each time-to-maturity.
# @keywords internal
# @author Bonsoo Koo and Kai-Yang Goh
#
num_points <- function(.data, ugrid, hu, qgrid, hq) {
  out <- matrix(0, nrow = length(ugrid), ncol = length(qgrid))
  u_idx <- calc_day_idx(.data, ugrid, hu)
  x_idx <- calc_tupq_idx(.data, qgrid, hq)
  #for(i in 1:length(ugrid)) {
  for(j in 1:length(qgrid)) {
    out[1, j] <- sum((.data$tupq %in% x_idx[j,1]:x_idx[j,2]))# &
    # (US_2$pqdate-min(US_2$pqdate) %in% u_idx[i,1]:u_idx[i,2]))
  }
  #}
  return(out)
}

# @title Calculates number of bonds that mature in each qgrid
# @param data A data frame; bond data to estimate discount curve from.
# @param ugrid A single value for ugrid between 0 and 1
# @param hu A single value for the bandwidth of the ugrid value
# @param qgrid A numeric vector of the time-to-maturity grid
# for the discount function at the corresponding time.
# @param hq A numeric vector matching qgrid, bandwidth parameter determining the size of the window
# that corresponds to each time-to-maturity.
# @param rgrid Optional, a single value for rgrid
# @param hr Optional, A single value for the bandwidth of the rgrid value for use with rgrid
# @param interest Optional, A vector of daily interest rates for use with rgrid
# @keywords internal
# @author Bonsoo Koo, Kai-Yang Goh and Nathaniel Tomasetti
#
num_points_mat <- function(data, ugrid, hu, qgrid, hq, rgrid, hr, interest, units = 365) {
  # Check dates in data matches interest rate
  dates <- unique(data$qdate)
  if(!missing(interest)){
    if(length(interest) != length(dates)){
      stop('Length of interest rate vector does not match number of unique qdates')
    }
  }
  # Calculate the u and r windows (if r is provided)

  window <- calc_uu_window(data, ugrid, hu)
  if(!missing(rgrid) & !missing(hr) & !missing(interest)){
    windowR <- calc_r_window(interest, rgrid, hr)
    window <- window * windowR
  }

  # Find subset of data with positive kernel
  kernel <- data.frame(qdate = dates, k = window)
  if("k" %in% colnames(data)) colnames(data)[colnames(data) == "k"] <- "original_k"
  data %>%
    left_join(kernel, by = 'qdate') %>%
    filter(.data$k > 0) -> dataSub

  # Calculate number of maturing bonds in each x window
  x_idx <- calc_tupq_idx(data, qgrid, hq, units)
  out <- rep(0, length(qgrid))
  for(j in 1:length(qgrid)) {
    out[j] <- sum((dataSub$tupq %in% x_idx[j,1]:x_idx[j,2]) & dataSub$pdint >= 100)
  }

  return(out)
}

#' Generate a yield curve with cubic time evolution
#' 
#' @details Returns a matrix where each column corresponds to a yield curve at a different point in time.
#' The initial curve at time to maturity zero is estimated from the following equation
#' \deqn{Yield_{i, 0} = b_0 + b_1 * ((1 - \exp(-\tau_i / t_1)) / (\tau / t_1)) + b_2 * ((1 - \exp(-\tau_i / t_2)) / (\tau_i / t2) - \exp(-\tau_i / t_2))}
#' where \eqn{\tau_i} is the index of the time to maturity period. This defines the yield curve for the quotation date = 0.
#' The yield curve for quotation dates = 1, 2, ... , max_q_date multiplies this curve by the cubic equation,
#' \deqn{Yield_{i, t} = Yield_{i, 0} * (1 + linear * t + quadratic * t^2 + cubic * t^3)}
#' so the yield curve slowly changes over different quotation dates.
#' 
#' @param max_qDate Integer giving the number of quotation dates to use in the data. Defaults to 12.
#' @param periods Integer giving the maximum number of time-to-maturity periods the yield curve is estimated for each quotation date. Defaults to 36
#' @param b0 First term in yield curve equation, Defaults to 0. See \code{Details}.
#' @param b1 Second term in yield curve equation, Defaults to 0.05. See \code{Details}.
#' @param b2 Third term in yield curve equation, Defaults to 2. See \code{Details}.
#' @param t1 Fourth term in yield curve equation, Defaults to 3. See \code{Details}.
#' @param t2 Fifth term in yield curve equation, Defaults to 500. See \code{Details}.
#' @param linear Linear term in yield curve evolution, Defaults to -0.55. See \code{Details}.
#' @param quadratic Quadratic term in yield curve evolution. Defaults to 0.55. See \code{Details}.
#' @param cubic Cubic term in yield curve evolution. Defaults to -0.55. See \code{Details}.
#' 
#' @return Matrix. Each column is a yield curve in a point in time (a quotation date). Each row is for a time-to-maturity.
#' For example, the number in the second column third row is the yield for the yield curve at the second quotation date,
#' for the third time-to-maturity ranking from shortest to longest. See \code{Details} for the equation to generate the yield curve.
#' See \code{Examples} for a example with the code to visually inspect the yield curves.
#' @examples
#' out <- generate_yield()
#' 
#' # plots
#' library(tidyverse)
#' out <- data.frame(out)
#' colnames(out) <- 1:12
#' out <- mutate(out, time = 1:36)
#' out <- pivot_longer(out, -time, names_to = "qdate", values_to = "yield")
#' ggplot(out) +
#'   geom_line(aes(x=time, y=yield, color = qdate))
#' 
generate_yield <- function(max_qDate = 12, periods = 36, b0 = 0, b1 = 0.05, b2 = 2, t1 = 3, t2 = 500,
                           linear = -0.55, quadratic = 0.55, cubic = -0.55){

  tauSeq <- (1:periods)/(periods/10)

  yieldInit <-    b0 + b1 * ((1 - exp(- tauSeq / t1)) / ( tauSeq / t1)) + b2 * ((1 - exp(- tauSeq / t2)) / (tauSeq / t2) - exp(- tauSeq / t2))

  yield <- matrix(0, periods, max_qDate)
  for(i in 1:max_qDate){
    t <- i / max_qDate
    yield[,i] <- yieldInit * (1 + cubic * t^3 + quadratic * t^2 + linear * t)
  }
  yield
}

#' Simulates data with sample data structure from which the yield can be estimated
#' 
#' Simulates data with the data structure that can be used in \code{estimate_yield}
#' 
#' The discount rate for each time to maturity and quotation date is calculated as
#' \deqn{Discount_{i, t} = \exp(-\tau_i * Yield_t),}
#' where \eqn{\tau_i} is the time-to-maturity of bond \eqn{i}.
#' After getting the discount rate for each quotation date and time to maturity, bonds are simulated and priced, where the price of Bond \eqn{j} on quotation date \eqn{t} is given by
#' \deqn{Price_{t, j} = \sum_i payment_i Discount_{i, t}}
#' A small error is added to each price. The resulting data may be input into \code{\link{estimate_yield}} with suitable grid values. This is shown in the vignette.
#' 
#' 
#' @param max_qDate Integer giving the number of quotation dates to use in the data. Defaults to 12.
#' @param periods Integer giving the maximum number of time to maturity periods the yield curve is estimated for each quotation date. Defaults to 36
#' @param bond_multiplier Integer giving the number of bonds to simulate for each day in the data.
#'  Total bonds equals multiplier * (periods + max_qdate). Defaults to 2
#' @param coupon_frequency Integer, frequency of coupon payments. Defaults to 3
#' @param coupon_rates Function without arguments to produce a vector of allowed coupon rates in percentage, each bond will have a coupon randomly drawn from this vector. Defaults to c(1, 2, 3, 4, 5). 
#' It is set to be a function so user can define different coupon rates based on maturity date of a bond \code{matDate}, an embedded integer.
#' @param sdev Standard deviation of the price errors, which is scaled by the square root of time to maturity so long term bonds have a larger error.
#' Defaults to 0.1
#' @param arma_terms List of ar and ma parameters passed to \code{arima.sim} in simulating error to add error dependency. Defaults to list(ar= 0.1, ma = 0). See \code{?stats::arima.sim}.
#' @param yield Matrix of yield curves at different time to maturities from \code{\link{generate_yield}}.
#' If NULL, a yield matrix is created from the default values of \code{\link{generate_yield}} with the values of \code{max_qDate} and \code{periods} input to \code{simulate_data()}.
#' 
#' @return Data frame with the following columns. See also \code{Details}.
#' \describe{
#'    \item{qdate}{The quotation date, as an integer}
#'    \item{crspid}{Factor that uniquely identifies the bond}
#'    \item{mid.price}{The quoted price of the bond on that day}
#'    \item{tupq}{Time until a given payment, given in days}
#'    \item{pdint}{Numeric value of payment}
#'    \item{tumat}{Time until maturity, in days (equal to tupq for zero coupon bonds)}
#'    \item{accint}{The accumulated interest on payments}
#' }
#' 
#' @examples
#' data <- simulate_data()
#' @export
simulate_data <- function(max_qDate = 12, 
                          periods = 36, 
                          bond_multiplier = 2, 
                          coupon_frequency = 3, 
                          coupon_rates = function(){1:5}, 
                          sdev = 0.1, 
                          arma_terms = list(ar = 0.1, ma = 0),
                          yield = NULL){


  tauSeq <- (1:periods)/(periods/10)
  qDates <- 1:max_qDate

  if(is.null(yield)){
    yield <- generate_yield(max_qDate, periods)
  }
  discount <- exp(-tauSeq * yield)


 # Keep nBonds as a multiple of (periods + max_qDate)

  nBonds <- bond_multiplier * (periods + max_qDate)

  ugrid <- (1:(max_qDate - 2))/(max_qDate - 1)
  hu <- rep(0.5 /  max_qDate, max_qDate -2)


  bondInfo <- data.frame()
  bondErrors <- data.frame()
  issues <- -c(1:coupon_frequency) + 1
  for(i in 1:nBonds){

    # ensure a bond matures on each day
    matDate <- i %% (periods + max_qDate) + 1



    issueDate <- issues[coupon_frequency + 1 -
                          ifelse(
                            matDate %% coupon_frequency == 0,
                            coupon_frequency,
                            matDate %% coupon_frequency
                          )
                        ]
    coupon <- sample(coupon_rates(), 1)


    if(coupon == 0){
      # Single payment on matdate
      payDates <- matDate
      pdint <- 100
    } else {

      payDates <- seq(issueDate, matDate, coupon_frequency)[-1]
      pdint <- c(rep(coupon, length(payDates) - 1), coupon + 100)
    }

    bondInfo <- rbind(bondInfo,
                      data.frame(crspid = as.character(i),
                             matdate = matDate,
                             pdint = pdint,
                             pqdate = payDates))

    bondErrors <- rbind(bondErrors,
                        data.frame(crspid = as.character(i),
                               qdate = qDates,
                               error = c(suppressWarnings(stats::arima.sim(arma_terms, max_qDate)))))
  }


  bondData <- list()
  for(q in 1:length(qDates)){
    qdate <- qDates[q]
    bondSub <- bondInfo[bondInfo$pqdate > qdate, ]
    bondSub$qdate <- qdate
    bondSub$tumat <- bondSub$matdate - qdate
    bondSub$tupq <- bondSub$pqdate - qdate
    bondSub <- bondSub[bondSub$tumat <= periods, ]

    disc <- data.frame(tupq = tauSeq,
                   d = discount[,q])

      bondSub <- merge(bondSub, disc, by = 'tupq')
      bondData[[q]] <- bondSub
    }
  bondData <-  do.call(rbind.data.frame, bondData)


  bondData <- merge(bondData, bondErrors, by = c('crspid', 'qdate'))
  bondData$sdev <- sdev * sqrt(bondData$tumat)

  prices <- lapply(with(bondData, split(bondData, paste(qdate, crspid, sdev))),
         function(x) data.frame(crspid = x$crspid[[1]],
                                qdate = x$qdate[[1]],
                                mid.price = sum(x$pdint * x$d) + mean(x$sdev + x$error)))
  bondData <- merge(do.call(rbind.data.frame, prices), bondData, by = c('qdate', 'crspid'))

  bondData$pqdate <- NULL
  bondData$d <- NULL
  bondData$error <- NULL
  bondData$sdev <- NULL
  bondData$matdate <- NULL
  bondData$accint <- 0

  bondData[order(bondData$qdate, as.numeric(as.character(bondData$crspid))), ]
}


