
#' Estimate yield function
#' 
#' @md
#' @description 
#' `r lifecycle::badge('experimental')`
#' 
#' Nonparametric estimation of discount functions at given dates, time-to-maturities, 
#' and interest rates (experienced users only) and their transformation to the yield curves.
#' 
#' @details 
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
#' \eqn{\hat{d}_k} depends on all the relevant discount values for the cash payments of the bond.
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
#' @param tau Numeric vector that
#' represents time-to-maturities in years where discount function and yield curve will be found
#' for each time point \code{xgrid}. 
#'  See \code{Details}.
#' @param ... Reserved for exogenous variables.
#' @param loess Logical. Whether the output estimated discount and yield are to be smoothed using locally estimated scatterplot smoothing (LOESS)
#' @examples 
#' library(dplyr)
#' # Simulate 4 bonds issued at 2020-01-01
#' # with maturity 180, 360, 540, 720 days
#' # Apart from the first one, 
#' # each has coupon 2, 
#' # of which half is paid every 180 days.
#' # The yield curve is sumulated fron `get_yield_at_vec`
#' # Quotation date is also at 2020-01-01
#' exp_data <- tibble(
#'   qdate = "2020-01-01", 
#'   crspid = rep(1:4, 1:4), 
#'   pdint = c(100, 1, 101, 1, 1, 101, 1, 1, 1, 101),
#'   tupq = unlist(sapply(1:4, seq_len)) * 180, 
#'   accint = 0
#' ) %>% 
#'   mutate(discount = exp(-tupq/365 * get_yield_at_vec(0, tupq/365))) %>% 
#'   group_by(crspid) %>% 
#'   mutate(mid.price = sum(pdint * discount)) %>% 
#'   ungroup()
#'   
#' # Only one quotation date so time grid is set to 1
#' xgrid <- 1 
#' # Discount function is evaluated at time to maturity of each payment in the data
#' tau <- unique(exp_data$tupq/365) 
#' 
#' ycevo(
#'   exp_data, 
#'   xgrid = xgrid,
#'   tau = tau
#' )
#' 
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a nonparametric model for bond prices from cross-section and time series information. Journal of Econometrics, 220(2), 562-588.
#' @order 1
#' @importFrom rlang enexpr
#' @importFrom lubridate days
#' @export
ycevo <- function(data, 
                  xgrid, 
                  tau, 
                  cols = NULL,
                  hx = NULL,
                  ht = NULL,
                  ...,
                  unit = days(1),
                  loess = length(tau)>10){
  
  if(anyDuplicated(xgrid)){
    stop("Duplicated xgrid found.")
  }
  if(anyDuplicated(tau)){
    stop("Duplicated tau found.")
  }
  d_col <- c('qdate', 'crspid', 'mid.price', 'accint', 'pdint', 'tupq')
  names(d_col) <- d_col
  cols <- enexpr(cols)
  
  if(!is.null(cols)){
    d_pairs <- as.list(cols)[-1]
    stopifnot(all(names(d_pairs) %in% d_col))
    s_col <- d_col
    s_col[names(d_pairs)] <- map_chr(d_pairs, as.character)
    data <- select(data, all_of(s_col))
    colnames(data) <- names(s_col)
  }
  
  if(is.null(hx))
    hx <- find_bindwidth_from_xgrid(xgrid, data)
  if(length(hx) == 1) hx <- rep(hx, length(xgrid))
  if(is.null(ht))
    ht <- find_bindwidth_from_tau(tau)
  
  estimate_yield(
    data = data ,
    xgrid = xgrid,
    hx = hx,
    tau = tau,
    ht = ht,
    loess = loess, 
    unit = unit)
}

find_bindwidth_from_tau <- function(tau){
  laggap <- tau - lag(tau)
  leadgap <- lead(tau) - tau
  vapply(
    1:length(tau), 
    function(x) max(laggap[x], leadgap[x], na.rm = TRUE), 
    numeric(1L))
}


find_bindwidth_from_xgrid <- function(xgrid, data){
  hx <- 1/length(xgrid)
  mat_weights_qdatetime <- get_weights(xgrid, hx, len = length(unique(data$qdate)))
  if(any(colSums(mat_weights_qdatetime) == 0)) {
    recommend <- seq_along(xgrid)/length(xgrid)
    stop("Inappropriate xgrid. Recommend to choose value(s) from: ", paste(recommend, collapse = ", "))
  }
  hx
}


