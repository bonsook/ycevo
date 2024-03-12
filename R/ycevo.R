
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
#' @param x Time grids at which the discount curve is evaluated. 
#' Should be specified using the same class of object as the quotation date (\code{qdate}) column in \code{data}.
#' @param tau Numeric vector that
#' represents time-to-maturities in years where discount function and yield curve will be found
#' for each time point \code{xgrid}. 
#'  See \code{Details}.
#' @param ... Reserved for exogenous variables.
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
#'   id = rep(1:4, 1:4), 
#'   pdint = c(100, 1, 101, 1, 1, 101, 1, 1, 1, 101),
#'   tupq = unlist(sapply(1:4, seq_len)) * 180, 
#'   accint = 0
#' ) %>% 
#'   mutate(discount = exp(-tupq/365 * get_yield_at_vec(0, tupq/365))) %>% 
#'   group_by(id) %>% 
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
                  x, 
                  hx = 1/length(x),
                  tau = NULL, 
                  ht = NULL,
                  tau_p = tau,
                  htp = NULL,
                  cols = NULL,
                  ...){
  stopifnot(is.data.frame(data))
  
  if(anyDuplicated(x)){
    stop("Duplicated time grid x found.")
  }
  if(anyDuplicated(tau)){
    stop("Duplicated tau found.")
  }
  
  
  stopifnot(!anyNA(x))
  stopifnot(!anyNA(tau))
  
  # The minimum required columns
  d_col <- c('qdate', 'id', 'mid.price', 'pdint', 'tupq')
  names(d_col) <- d_col
  cols <- enexpr(cols)
  # Now use id, not crspid
  if(any(colnames(data) == "crspid")) 
    warning('Column name "crspid" is deprecated. Column "id" is now used as asset identifier.')
  # If user specify cols, replace the columns in the data with cols
  qdate_label <- "qdate"
  s_col <- d_col
  if(!is.null(cols)){
    d_pairs <- as.list(cols)[-1]
    stopifnot(all(names(d_pairs) %in% d_col))
    s_col[names(d_pairs)] <- vapply(d_pairs, as.character, character(1))
    qdate_label <- s_col[qdate_label]
  }
  
  dots <- enexprs(...)
  if(length(dots) > 1)
    stop("Currently only supports one extra predictor (interest rate)")
  if(length(names(dots)) > 0) 
    stop("The input of ... cannot be named")
  names(dots) <- vapply(dots, as.character, FUN.VALUE = character(1L))
    
  # Minimal data
  data <- select(data, all_of(s_col), any_of(names(dots)))
  colnames(data) <- c(names(s_col), names(dots))
  
  if(any(temp <- (!names(dots) %in% colnames(data)))){
    stop(paste0(names(dots)[temp], collapse = ", "), " column(s) not found in the data")
  }
  
  xgrid <- ecdf(data$qdate)(x)
  
  # Handle interest rate
  interest <- NULL
  rgrid <- NULL
  hr <- NULL
  if(length(dots) > 0){
    
    interest <- data %>% 
      select(all_of(c("qdate", names(dots)))) %>% 
      arrange(qdate) %>% 
      distinct(qdate, .keep_all = TRUE) %>% 
      pull(names(dots))
    rgrid <- dots[[1]][[1]]
    hr <- dots[[1]][[2]]
    rgrid_order <- order(rgrid)
    rgrid <- rgrid[rgrid_order]
    hr <- hr[rgrid_order]
    
    stopifnot(identical(length(xgrid), length(rgrid)))
  }
  
  # Handle grids
  # xgrid and hx
  hx <- check_hx(xgrid, hx, data)
  if(length(hx) == 1) {
    hx <- rep(hx, length(xgrid))
  } else if(length(hx) != length(xgrid)) {
    stop("Length of hx does not equal to length of x.")
  }
  # tau 
  if(is.null(tau)) {
    max_tupq <- max(data$tupq)
    tau <- seq_tau(max_tupq/365)
  }
  # ht
  if(is.null(ht))
    ht <- find_bindwidth_from_tau(tau)
  # ht
  if(is.null(htp))
    htp <- find_bindwidth_from_tau(tau_p)
  
  if(is.vector(ht))
    ht <- matrix(ht, nrow = length(ht), ncol = length(xgrid))
  if(is.vector(htp))
    htp <- matrix(htp, nrow = length(htp), ncol = length(xgrid))
  
  # sort xgrid and tau
  # in case the user don't specify them in sorted order
  order_xgrid <- order(xgrid)
  order_tau <- order(tau)
  order_tau_p <- order(tau_p)
  xgrid <- xgrid[order_xgrid]
  hx <- hx[order_xgrid]
  tau <- tau[order_tau]
  ht <- as.matrix(ht)[order_tau, order_xgrid, drop = FALSE]
  tau_p <- tau[order_tau_p]
  htp <- as.matrix(htp)[order_tau_p, order_xgrid, drop = FALSE]
  
  
  # Handle tau and tau_p again
  # based on number of bonds in each window
  tau_adjusted <- mapply(create_tau_ht,
    xgrid = xgrid,
    hx = hx,
    ht = asplit(ht, 2),
    htp = asplit(htp, 2),
    MoreArgs = list(
      data = data, tau = tau, tau_p = tau_p, 
      rgrid = rgrid, hr = hr, interest = interest),
    SIMPLIFY = FALSE
  )
  
  pb <- progressr::progressor(length(xgrid))
  output <- future.apply::future_lapply(
    seq_along(xgrid),
    function(i) {
      on.exit(pb())
      dplyr::rename(estimate_yield(
        data = data ,
        xgrid = xgrid[[i]],
        hx = hx[[i]],
        tau = tau_adjusted[[i]]$tau,
        ht = tau_adjusted[[i]]$ht,
        tau_p = tau_adjusted[[i]]$tau_p, 
        htp = tau_adjusted[[i]]$htp,
        rgrid = rgrid[[i]],
        hr = hr[[i]], 
        interest = interest),
        .discount = discount, .yield = yield)
    },
    future.seed = TRUE
  ) 
  
  res <- output %>% 
    bind_rows() %>% 
    dplyr::relocate(any_of(c("xgrid", "rgrid", "tau", ".discount", ".yield"))) %>% 
    tidyr::nest(.est = c(tau, .discount, .yield)) %>% 
    rename_with(function(x) rep(names(dots) %||% character(0), length(x)), any_of("rgrid")) %>% 
    mutate(!!sym(qdate_label) := x, .before = 1) %>% 
    select(-xgrid)
  
  attr(res, "cols") <- cols
  attr(res, "qdate_label") <- qdate_label
  new_ycevo(res)
}

check_hx <- function(xgrid, hx, data){
  
  if(isTRUE(all.equal(hx, 1/length(xgrid)))) {
    num_qdate <- length(unique(data$qdate))
    mat_weights_qdatetime <- get_weights(xgrid, hx, len = num_qdate)
    if(any(colSums(mat_weights_qdatetime) == 0)) {
      recommend <- seq_along(xgrid)/length(xgrid)
      stop("Inappropriate xgrid. Recommend to choose value(s) from: ", paste(recommend, collapse = ", "))
    }
  }
  
  hx
}

#' @export
seq_tau <- function(max_tau) {
  tau <-  c(seq(30, 6 * 30, 30),  # Monthly up to six months
            seq(240, 2 * 365, 60),  # Two months up to two years
            seq(720 + 90, 6 * 365, 90),  # Three months up to six years
            seq(2160 + 120, 20 * 365, 120),  # Four months up to 20 years
            #               seq(20 * 365 + 182, 30 * 365, 182)) / 365 # Six months up to 30 years
            seq(20 * 365 + 182, 30.6 * 365, 182)) / 365
  tau[tau < max_tau]
}

find_bindwidth_from_tau <- function(tau){
  laggap <- tau - lag(tau)
  leadgap <- lead(tau) - tau
  vapply(
    1:length(tau), 
    function(x) max(laggap[x], leadgap[x], na.rm = TRUE), 
    numeric(1L))
}




new_ycevo <- function(x) {
  structure(x, class = c("ycevo", class(x)))
}
