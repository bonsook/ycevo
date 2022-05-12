
#' Estimate yield function
#' 
#' Nonparametric estimation of discount functions at given dates, time-to-maturities, 
#' and interest rates (experienced users only) and their transformation to the yield curves.
#' 
#' 
#' Suppose that a bond \eqn{i} has a price \eqn{p_i} at time t with a set of cash payments, 
#' say \eqn{c_1, c_2, \ldots, c_m} with a set of corresponding discount values
#' \eqn{d_1, d_2, \ldots, d_m}. In the bond pricing literature, the market price of 
#' a bond should reflect the discounted value of cash payments. Thus, we want to minimise
#' \deqn{(p_i-\sum^m_{j=1}c_j\times d_j)^2.}
#' For the estimation of \eqn{d_k(k=1, \ldots, m)}, solving the first order condition yields
#' \deqn{(p_i-\sum^m_{j=1}c_j \times d_j)c_k = 0, }
#' and
#' \deqn{\hat{d}_k = \frac{p_i c_k}{c_k^2} - \frac{\sum^m_{j=1,k\neq k}c_k c_j d_j}{c_k^2}.}
#' 
#' There are challenges:
#' \eqn{\hat{d}_k} depends on all the relevant disount values for the cash payments of the bond.
#' Our model contains random errors and our interest lies in expected value of \eqn{d(.)} where 
#' the expected value of errors is zero.
#' \eqn{d(.)} is an infinite-dimensional function not a discrete finite-dimensional vector.
#' Generally, cash payments are made biannually, not dense at all. Moreover, cash payment schedules
#' vary over different bonds.
#' 
#' Let \eqn{d(\tau, X_t)} be the discount function at given covariates \eqn{X_t} (dates \code{xgrid} and 
#' interest rates \code{rgrid}), and given time-to-maturities \eqn{\tau} (\code{tau}).
#' \eqn{y(\tau, X_t)} is the yield curve at given covariates \eqn{X_t} (dates \code{xgrid} and 
#' interest rates \code{rgrid}), and given time-to-maturities \eqn{\tau} (\code{tau}).
#' 
#' We pursue the minimum of the following 
#' smoothed sample least squares objective function for any smooth function \eqn{d(.)}:
#' \deqn{Q(d) = \sum^T_{t=1}\sum^n_{i=1}\int\{p_{it}-\sum^{m_{it}}_{j=1}c_{it}(\tau_{ij})d(s_{ij}, x)\}^2 \sum^{m_{it}}_{k=1}\{K_h(s_{ik}-\tau_{ik})ds_{ik}\}K_h(x-X_t)dx,}
#' where a bond \eqn{i} has a price \eqn{p_i} at time t with a set of cash payments \eqn{c_1, c_2, \ldots, c_m} with a set of corresponding discount values
#' \eqn{d_1, d_2, \ldots, d_m},
#' \eqn{K_h(.) = K(./h)} is the kernel function with a bandwidth parameter \eqn{h},
#' the first kernel function is the kernel in space with bonds whose maturities \eqn{s_{ik}}
#' are close to the sequence \eqn{\tau_{ik}}, 
#' the second kernel function is the kernel in time and in interest rates with \eqn{x},  
#' which are close to the sequence \eqn{X_t}.
#' This means that bonds with similar cash flows, and traded in contiguous days, 
#' where the short term interest rates in the market are similar,
#' are combined for the estimation of the discount function at a point in space, in time, and in "interest rates".
#' 
#' The estimator for the discount function over time to maturity and time is
#' \deqn{\hat{d}=\arg\min_d Q(d).}
#' This function provides a data frame of the estimated yield and discount rate at each combination of the 
#' provided grids. The estimated yield is transformed from the estimated discount rate.
#' 
#' For more information on the estimation method, please refer to \code{References}.
#'
#' 
#' 
#' @param data Data frame; bond data to estimate discount curve from. See \code{?USbonds} for an example bond data structure.
#' @param xgrid Numeric vector of values between 0 and 1. 
#' Time grids over the entire time horizon (percentile) of the data at which the discount curve is evaluated.
#' @param tau Numeric vecter that
#' represents time-to-maturities in years where discount function and yield curve will be found
#' for each time point \code{xgrid}. 
#'  See \code{Details}.
#' @param ... Reserved for exogenous variables.
#' @param loess (Optional) Logical. Whether the output estimated discount and yield are to be smoothed using locally estimated scatterplot smoothing (LOESS)
#' @order 1
#' @export
ycevo <- function(data, 
                  xgrid, 
                  tau, 
                  ...,
                  loess = TRUE){
  
  if(anyDuplicated(xgrid)){
    stop("Duplicated xgrid found.")
  }
  if(anyDuplicated(tau)){
    stop("Duplicated tau found.")
  }
  
  hx <- find_bindwidth_from_xgrid(xgrid, data)
  ht <- find_bindwidth_from_tau(tau)
  
  estimate_yield(
    data = bond ,
    xgrid = xgrid,
    hx = hx,
    tau = tau,
    ht = ht,
    loess = loess)
}

find_bindwidth_from_tau <- function(tau){
  laggap <- tau - lag(tau)
  leadgap <- lead(tau) - tau
  vapply(
    1:length(tau), 
    function(x) max(laggap[x], leadgap[x], na.rm = TRUE), 
    1)
}


find_bindwidth_from_xgrid <- function(xgrid, data){
  hx <- 1/length(xgrid)
  if(sum(calc_uu_window(data, xgrid, hx)) == 0) {
    recommend <- seq_along(xgrid)/length(xgrid)
    stop("Inappropriate xgrid. Recommend to choose value(s) from: ", paste(recommend, collapse = ", "))
  }
  hx
}


