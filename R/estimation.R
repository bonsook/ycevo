prep_windows <- function(data, xgrid, hx, rgrid, hr, 
                         tau, ht, tau_p, htp, 
                         interest, units, interest_grid){
  # windows etc
  day_idx <- calc_day_idx(data, xgrid, hx)
  uu_window <- calc_uu_window(data,xgrid,hx)
  nday <- length(xgrid)
  joint_window <-  NULL
  if(interest_grid){
    r_window <- calc_r_window(interest, rgrid, hr)
    day_grid <- expand.grid(ug = xgrid, rg = rgrid)
    nday <- nrow(day_grid)
    joint_window <- matrix(0, nrow(uu_window), nday)
    for(j in seq_along(rgrid)){
      for(i in seq_along(xgrid)){
        joint_window[, (j-1)*length(xgrid) + i] <- uu_window[,i] * r_window[,j]
      }
    }
    apply(joint_window, 2, function(y) {
      if(all(y == 0)){
        day_idx <- c(0, 0)
      } else {
        window_idx <- which(y != 0)
        day_idx <- c(window_idx[1], window_idx[length(window_idx)])
      }
      day_idx
    }) %>% t() -> day_idx
    obs <- which(day_idx[,1] != 0)
    day_grid <- day_grid[obs, ]
    joint_window <- joint_window[,obs]
    day_idx <- day_idx[obs, ]
    nday <- length(obs)
    if(nday == 1){
      joint_window <- matrix(joint_window, ncol = 1)
      day_idx <- matrix(day_idx, nrow = 1)
    }
    
  }
  
  
  if(is.vector(tau)){
    tupq_idx_x <- calc_tupq_idx(data, tau, ht, units)
    ux_window <- calc_ux_window(data, tau, ht, units)
    ntupq_x <- length(tau)
  } else if(nrow(tau) == 1){
    tupq_idx_x <- calc_tupq_idx(data, tau, ht, units)
    ux_window <- calc_ux_window(data, tau, ht, units)
    ntupq_x <- length(tau)
  } else {
    tupq_idx_x <- NULL
    ux_window <- NULL
    ntupq_x <- ncol(tau)
    for(i in 1:nrow(tau)){
      for(i in 1:nrow(tau)){
        if(is.vector(ht)){
          tupq_idx_x <- cbind(tupq_idx_x, calc_tupq_idx(data, tau[i,], ht, units))
          ux_window <- cbind(ux_window, calc_ux_window(data, tau[i, ], ht, units))
        } else if(nrow(ht) == 1){
          tupq_idx_x <- cbind(tupq_idx_x, calc_tupq_idx(data, tau[i,], ht, units))
          ux_window <- cbind(ux_window, calc_ux_window(data, tau[i, ], ht, units))
        } else if(nrow(ht) == nrow(tau)){
          tupq_idx_x <- cbind(tupq_idx_x, calc_tupq_idx(data, tau[i,], ht[i, ], units))
          ux_window <- cbind(ux_window, calc_ux_window(data, tau[i, ], ht[i, ], units))
        } else {
          stop('the dimension of ht must match the dimension of tau')
        }
      }
    }
  }
  
  if(is.vector(tau_p)){
    tupq_idx_q <- calc_tupq_idx(data, tau_p, htp, units)
    uq_window <- calc_ux_window(data, tau_p, htp, units)
    ntupq_q <- length(tau_p)
  } else if(nrow(tau_p) == 1){
    tupq_idx_q <- calc_tupq_idx(data, tau_p, htp, units)
    uq_window <- calc_ux_window(data, tau_p, htp, units)
    ntupq_q <- length(tau_p)
  } else {
    tupq_idx_q <- NULL
    uq_window <- NULL
    ntupq_q <- ncol(tau_p)
    for(i in 1:nrow(tau_p)){
      for(i in 1:nrow(tau_p)){
        if(is.vector(htp)){
          tupq_idx_q <- cbind(tupq_idx_q, calc_tupq_idx(data, tau_p[i,], htp, units))
          uq_window <- cbind(uq_window, calc_ux_window(data, tau_p[i, ], htp, units))
        } else if(nrow(htp) == 1){
          tupq_idx_q <- cbind(tupq_idx_q, calc_tupq_idx(data, tau_p[i,], htp, units))
          uq_window <- cbind(uq_window, calc_ux_window(data, tau_p[i, ], htp, units))
        } else if(nrow(htp) == nrow(tau_p)){
          tupq_idx_q <- cbind(tupq_idx_q, calc_tupq_idx(data, tau_p[i,], htp[i, ], units))
          uq_window <- cbind(uq_window, calc_ux_window(data, tau_p[i, ], htp[i, ], units))
        } else {
          stop('the dimension of htp must match the dimension of tau_p')
        }
      }
    }
  }
  
  list(day_idx = day_idx, 
       joint_window = joint_window, 
       nday = nday, 
       ntupq_x = ntupq_x, 
       ntupq_q = ntupq_q, 
       tupq_idx_x = tupq_idx_x, 
       tupq_idx_q = tupq_idx_q, 
       uu_window = uu_window, 
       ux_window = ux_window, 
       uq_window = uq_window)
}



# A component of the \code{estimate_yield} function that calculates \eqn{dbar}
# 
# Internal function that estimates the discount function without taking into account
# cross products of coupon payments. Not to be used independently. Exported for documentation purpose.
# 
# @param data A data frame; bond data to estimate discount curve from. See \code{?USbonds} for an example bond data structure.
# @param xgrid A length T numeric vector; the times at which the discount curve will be estimated.
# @param hx A length T numeric vector, bandwidth parameter determining the size of the window
# that corresponds to each time at which the discount curve is estimated,
# @param rgrid (Optional) A length K numeric vector of interest rate grid values
# @param hr (Optional) A length K numeric vector of interest rate grid bandwidths
# @param tau A length m numeric vector, or either a 1 x x or T x x numeric matrix. If a T x x matrix, each row represents the time-to-maturity grid
# that each time-to-maturity in the discount function is compared against. Otherwise the same time-to-maturity grid is repeated for each of the T xgrid values
# @param ht An numeric vector of length T, or if tau is a matrix, a numeric matrix of the same dimensions as tau.
# A vector ht for a T x m matrix tau repeats values for each row of tau_p.
# bandwidth parameter determining the size of the window that corresponds to each time-to-maturity.
# @param price_slist (Optional) A list of matrices, generated by calc_price_slist.
# @param cf_slist (Optional) A list of matrices, generated by calc_cf_slist.
# @param interest (Optional) A vector of daily short term interest rates
# @param units (Optional) number of tupq per tau (e.g. 365 for daily data with annual grid values). Defaults to 365
# @param unit (Optional) Smallest interval between quotation date time. Class Period. Needs to be by which 365 (days) is divided exactly. 
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
# @source Koo, B., La Vecchia, D., & Linton, O. B. (2019). Estimation of a Nonparametric model for Bond Prices from Cross-section and Time series Information. Available at SSRN3341344.
# @author Nathaniel Tomasetti
# 
calc_dbar <- function(data, xgrid, 
                      tau, 
                      price_slist, cf_slist, 
                      interest_grid, windows_ls) {
  day_idx <- windows_ls$day_idx
  tupq_idx <- windows_ls$tupq_idx_x
  ux_window <- windows_ls$ux_window
  uu_window <- windows_ls$uu_window
  joint_window <- windows_ls$joint_window
  ntupq <- windows_ls$ntupq_x
  nday <- windows_ls$nday
  
  
  if(interest_grid){
    dbar <- calc_dbar_c(nday, ntupq, day_idx, tupq_idx, ux_window, joint_window, price_slist, cf_slist)
    day_grid <- day_grid[rep(1:nday, each=ntupq),]
    dbar <- data.frame(ug = day_grid$ug, rg = day_grid$rg, dbar_numer = dbar[,1], dbar_denom = dbar[,2])
  } else {
    dbar <- calc_dbar_c(nday, ntupq, day_idx, tupq_idx, ux_window, uu_window, price_slist, cf_slist)
    dbar <- data.frame(ug = rep(xgrid, rep(ntupq, nday)), dbar_numer = dbar[,1], dbar_denom = dbar[,2])
  }
  if(is.vector(tau)){
    dbar$xg = rep(tau, nday)
  } else if(nrow(tau) == 1) {
    dbar$xg = rep(tau, nday)
  } else {
    dbar$xg = c(t(tau))
  }
  dbar
}

# A component of the \code{estimate_yield} function that calculates \eqn{hhat}
# 
# Internal function that calculates coupon payment cross products. 
# Not to be used independently. Exported for documentation purpose.
# 
# @param data A data frame; bond data to estimate discount curve from. See \code{?USbonds} for an example bond data structure.
# @param xgrid A length T numeric vector; the times at which the discount curve will be estimated.
# @param hx A length T numeric vector, bandwidth parameter determining the size of the window
# that corresponds to each time at which the discount curve is estimated,
# @param rgrid (Optional) A length K numeric vector of interest rate grid values
# @param hr (Optional) A length K numeric vector of interest rate grid bandwidths
# @param tau A length m numeric vector, or either a 1 x m or T x m numeric matrix. If a T x m matrix, each row represents the time-to-maturity grid
# for the discount function at the corresponding time. Otherwise the same time-to-maturity grid is repeated for each of the T xgrid values
# @param ht An numeric vector of length T, or if tau is a matrix, a numeric matrix of the same dimensions as tau.
# A vector ht for a T x m matrix tau_p repeats values for each row of tau.
# bandwidth parameter determining the size of the window that corresponds to each time-to-maturity.
# @param tau_p (Optional), A length m numeric vector, or either a 1 x m or T x m numeric matrix, matching tau input with m allowed to be different to M.
# If a T x m matrix, each row represents the time-to-maturity grid that each time-to-maturity in the discount function is compared against.
# Otherwise the same time-to-maturity grid is repeated for each of the T xgrid values.
# If m = M, and each entry of tau_p is identical to tau, estimation is performed without interpolaton of the h-hat matrix.
# If the entries are not identical each entry of each row of tau_p must be within the range from the smallest to largest value of the corresponding row of tau,
# and linear interpolation of the h-hat matrix is performed.
# If omitted, tau_p is set equal to tau.
# @param htp (Optional) An numeric vector of length T, or if tau_p is a matrix, a numeric matrix of the same dimensions as tau_p.
# A vector htp for a T x M matrix tau_p repeats values for each row of tau_p.
# bandwidth parameter determining the size of the window that corresponds to each time-to-maturity.
# If omitted, htp is set equal to ht
# @param cf_slist (Optional) A list of matrices, generated by calc_cf_slist.
# @param interest (Optional) A vector of daily short term interest rates
# @param units (Optional) number of tupq per tau (e.g. 365 for daily data with annual grid values). Defaults to 365
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
# @author Nathaniel Tomasetti
# @source Koo, B., La Vecchia, D., & Linton, O. B. (2019). Estimation of a Nonparametric model for Bond Prices from Cross-section and Time series Information. Available at SSRN3341344.
# 
calc_hhat_num <- function(data, xgrid, 
                          tau, 
                          tau_p = tau, 
                          cf_slist = NULL, 
                          interest_grid, windows_ls) {
  day_idx <- windows_ls$day_idx
  tupq_idx_x <- windows_ls$tupq_idx_x
  tupq_idx_q <- windows_ls$tupq_idx_q
  ux_window <- windows_ls$ux_window
  uu_window <- windows_ls$uu_window
  uq_window <- windows_ls$uq_window
  joint_window <- windows_ls$joint_window
  ntupq_x <- windows_ls$ntupq_x
  ntupq_q <- windows_ls$ntupq_q
  nday <- windows_ls$nday
  
  
  if(interest_grid){
    hhat <- calc_hhat_num2_c(nday, ntupq_x, ntupq_q, day_idx, tupq_idx_x, tupq_idx_q, ux_window, uq_window, joint_window, cf_slist)
    day_grid <- day_grid[rep(1:nday, each=ntupq_q*ntupq_x),]
    hhat <- data.frame(hhat_numer = c(hhat), ug = day_grid$ug, rg = day_grid$rg)
  } else {
    hhat <- calc_hhat_num2_c(nday, ntupq_x, ntupq_q, day_idx, tupq_idx_x, tupq_idx_q, ux_window, uq_window, uu_window, cf_slist)
    hhat <- data.frame(hhat_numer = c(hhat), ug = rep(xgrid, rep(ntupq_q * ntupq_x, nday)))
  }
  
  if(is.vector(tau)){
    hhat$xg = rep(tau, nday*ntupq_q)
  } else if(nrow(tau) == 1){
    hhat$xg = rep(tau, nday*ntupq_q)
  } else {
    x_temp = NULL
    for(i in 1:nday){
      x_temp = c(x_temp, rep(tau[i,], ntupq_q))
    }
    hhat$xg = x_temp
  }
  
  if(is.vector(tau_p)){
    hhat$qg = rep(tau_p, rep(ntupq_x, ntupq_q))
  } else if(nrow(tau_p) == 1){
    hhat$qg = rep(tau_p, rep(ntupq_x, ntupq_q))
  } else {
    q_temp = NULL
    for(i in 1:nday){
      q_temp = c(q_temp, rep(tau_p[i,], rep(ntupq_x, ntupq_q)))
    }
    hhat$qg = q_temp
  }
  
  hhat
}

#' @param hx Numeric vector of values between 0 and 1. Bandwidth parameter determining the size of the window 
#' that corresponds to each time point (\code{xgrid}).
#' See \code{Details}.
#' The selection of bandwidth parameter is crucial in non-parametric estimation.
#' If not sure, please use \code{ycevo} to allow the function choose it for you.
#' @param ht Numeric vector that
#' represents bandwidth parameter determining the size of the window 
#' in the kernel function
#' that corresponds to each time-to-maturities (\code{tau}). 
#' The same unit as \code{tau}.
#' See \code{Details}.
#' The selection of bandwidth parameter is crucial in non-parametric estimation.
#' If not sure, please use \code{ycevo} to allow the function choose it for you.
#' @param rgrid (Optional) Numeric vector of interest rate grids in percentage at which the discount curve is evaluated, e.g. 4.03 means at interest rate of 4.03\%.
#' @param hr (Optional) Numeric vector of bandwidth parameter in percentage determining the size of the window
#' in the kernel function
#' that corresponds to each interest rate grid (\code{rgrid}).
#' @param interest (Optional) Numeric vector of daily short term interest rates. 
#' The length is the same as the number of quotation dates included in the data, 
#' i.e. one interest rate per day.
#' @param units (Optional) number of tupq per tau (e.g. 365 for daily data with annual grid values). Defaults to 365
#' @param cfp_slist (Internal) Experienced users only. A list of matrices, generated by the internal function \code{get_cfp_slist}.
#' 
#' @return Data frame of the yield and discount rate at each combination of the provided grids.
#' \describe{
#'   \item{discount}{Estimated discount rate}
#'   \item{xgrid}{Same as input \code{xgrid}}
#'   \item{tau}{Same as input \code{tau}}
#'   \item{yield}{Estimated yield}
#' }
#' 
#' @author Nathaniel Tomasetti, Bonsoo Koo, and Yangzhuoran Fin Yang
#' @describeIn ycevo Experienced users only. 
#' Yield estimation with interest rate and manually selected bandwidth parameters.
#' @export
estimate_yield <- function(data, xgrid, hx, tau, ht, rgrid = NULL, hr = NULL, interest = NULL, loess = TRUE, 
                            cfp_slist = NULL,
                           units = 365){
  # units <- 365
  tau_p <- NULL 
  htp <- NULL
  if(is.null(tau_p) || is.null(htp)){
    tau_p <- tau
    htp <- ht
  }
  if(!is.null(rgrid) & !is.null(hr) & !is.null(interest)){
    interest_grid <- TRUE
  } else {
    interest_grid <- FALSE
  }
  
  # Check inputs
  if(!is.vector(xgrid)){
    stop('xgrid must be a vector')
  }
  if(!is.vector(hx)){
    stop('hx must be a vector')
  }
  if(!is.data.frame(data)){
    stop('data must be a dataframe')
  }
  if(!all(c('qdate', 'crspid', 'mid.price', 'accint', 'pdint', 'tupq') %in% colnames(data))){
    stop('data must contain columns qdate, crspid, mid.price, accint, pdint, and tupq')
  }
  if(!is.matrix(tau) & !is.vector(tau)){
    stop('tau must be a vector or a matrix')
  }
  if(!is.numeric(xgrid)){
    stop('xgrid must be numeric')
  }
  if(!is.numeric(hx)){
    stop('hx must be numeric')
  }
  if(!is.numeric(tau)){
    stop('tau must be numeric')
  }
  if(length(xgrid) != length(hx)){
    stop('xgrid and hx must have the same length')
  }
  if(is.vector(tau)){
    if(is.vector(ht)){
      if(length(ht) != length(tau)){
        stop('ht and tau must have the same length')
      }
    } else {
      stop('ht must be a vector of xgrid is a vector')
    }
  }
  if(is.matrix(tau)){
    if(is.vector(ht)){
      if(length(ht) != ncol(tau)){
        stop('a vector ht must have a length equal to the number of columns of tau')
      }
    } else {
      if(ncol(ht) != ncol(tau) | nrow(ht) != nrow(tau)){
        stop('a matrix ht must have the same dimensions as tau')
      }
    }
  }
  
  
  if(is.null(cfp_slist)){
    cfp_slist <- get_cfp_slist(data)
    cf_slist <- cfp_slist$cf_slist
    price_slist <- cfp_slist$price_slist
  }
  # browser()
  
  windows_ls <- prep_windows(data = data, 
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
                             interest_grid = interest_grid)
  # Estimate dbar & the numerator of the h-hat matrix
  dbar <- calc_dbar(data = data,
                    xgrid = xgrid,
                    tau = tau,
                    price_slist = price_slist,
                    cf_slist = cf_slist,
                    interest_grid = interest_grid, 
                    windows_ls = windows_ls)
  
  hhat_num <- calc_hhat_num(data = data,
                            xgrid = xgrid,
                            tau = tau,
                            tau_p = tau_p,
                            cf_slist = cf_slist,
                            interest_grid = interest_grid, 
                            windows_ls = windows_ls)
  
  if(any(dbar$dbar_denom == 0)) {
    problem_tau <- filter(dbar, .data$dbar_denom == 0)$xg 
    warning("tau values at ", paste(problem_tau, collapse = ", "), " does not have enough obs to estimate yield")
    
    output <- estimate_yield(
      data = data,
      xgrid = xgrid,
      hx = hx,
      tau = tau[!tau %in% problem_tau],
      ht = ht[!tau %in% problem_tau], 
      loess = FALSE)
    return(output)
  }
  
  
  
  # The denominator of h-hat entries are estimated as part of dbar
  if(interest_grid){
    
    hhat <- dplyr::mutate(
      dplyr::right_join(
        dplyr::select(dbar, !!sym('ug'), !!sym('xg'), !!sym('rg'), !!sym('dbar_denom')),
        hhat_num,
        by = c('ug' = 'ug', 'xg' = 'xg', 'rg' = 'rg')
      ),
      hhat = !!sym('hhat_numer') / !!sym('dbar_denom')
    )
    
    day_grid <- unique(hhat[c('ug', 'rg')])
  } else {
    hhat <- dplyr::mutate(
      dplyr::right_join(
        dplyr::select(dbar, !!sym('ug'), !!sym('xg'), !!sym('dbar_denom')),
        hhat_num,
        by = c('ug' = 'ug', 'xg' = 'xg')
      ),
      hhat =!!sym('hhat_numer') / !!sym('dbar_denom')
    )
    day_grid <- data.frame(ug = unique(hhat$ug), rg = 0)
    hhat$rg <- 0
    dbar$rg <- 0
  }
  
  # Create the dataframe that the function returns
  dhat <- data.frame()
  for(i in 1:nrow(day_grid)){
    
    # Create the dbar vector and h-hat matrix for this value of ugrid
    db <- dplyr::mutate(
      dplyr::filter(dbar, !!sym('ug') == day_grid$ug[i], !!sym('rg') == day_grid$rg[i]),
      dbar = !!sym('dbar_numer') / !!sym('dbar_denom')
    )$dbar
    
    hh <- matrix(dplyr::filter(hhat, !!sym('ug') == day_grid$ug[i], !!sym('rg') == day_grid$rg[i])$hhat,
                 nrow = length(db))
    if(any(is.na(db))){
      na <- which(is.na(db))
      if(is.vector(tau) & is.vector(tau_p)){
        xgr <- tau[-na]
        qgr <- tau_p[tau_p <= max(xgr)]
      } else if(nrow(tau) == 1 & nrow(tau_p) == 1){
        xgr <- tau[-na]
        qgr <- tau_p[tau_p <= max(xgr)]
      } else {
        xgr <- tau[i,-na]
        qgr <- tau_p[i, tau_p[tau_p <= max(xgr)]]
      }
      db <- db[-na]
      hh <- hh[1:length(xgr), 1:length(qgr)]
    } else {
      if(is.vector(tau) & is.vector(tau_p)){
        xgr <- tau
        qgr <- tau_p
      } else if(nrow(tau) == 1 & nrow(tau_p) == 1){
        xgr <- tau
        qgr <- tau_p
      } else {
        xgr <- tau[i, ]
        qgr <- tau_p[i, ]
      }
    }
    
    
    # Extract the xgrid and qgrid for this value of u as xgr and qgr. 
    # The dimensions of these objects are tested earlier in the code.
    
    # Create a matrix of interpolation weights
    interpol_weights <- matrix(0, length(xgr), length(qgr))
    
    # Iterate over the values of qgrid
    for(j in 1:length(qgr)){
      # If qgrid is contained in xgrid then the weight will be one
      if(any(xgr == qgr[j])){
        interpol_weights[which(xgr == qgr[j]), j] <- 1
      } else {
        # Otherwise find the xgrid immediately above and below this xgrid
        lower <- max(which(xgr < qgr[j]))
        upper <- min(which(xgr > qgr[j]))
        # Error if qgrid is lower than the first xgrid, or greater than the last xgrid
        if(upper == 1 | lower == length(xgr)){
          stop('tau_p entries must lie inside tau')
        }
        # Find interpolation weights as ratio between the two xgrid values lying above and below this qgrid
        dist <- xgr[upper] - xgr[lower]
        interpol_weights[lower, j] <- (qgr[j] - xgr[lower]) / dist
        interpol_weights[upper, j] <- (xgr[upper] - qgr[j]) / dist
      }
    }
    
    # Construct the length(q) x length(x) matrix of the interpolated hhat
    hh_interpol <- matrix(0, length(xgr), length(xgr))
    for(j in 1:length(xgr)){
      hh_interpol[,j] <- colSums(t(hh) * interpol_weights[j,])
    }
    
    # transpose?
    X <- diag(1, length(xgr)) + t(hh_interpol)
    dh <- solve(X) %*% db
    if(interest_grid){
      dhat <- rbind(dhat, data.frame(discount = dh, ug = day_grid$ug[i], rgrid = day_grid$rg[i], qg = xgr))
    } else {
      dhat <- rbind(dhat, data.frame(discount = dh, ug = day_grid$ug[i], qg = xgr))
    }
    
  }
  # loess smoothing
  if(loess){
    # loess_model <- stats::loess(discount~qg, data = dhat)
    # dhat$discount <- predict(loess_model)
    loess_model <- lapply(unique(dhat$ug), 
                          function(ugg) stats::loess(discount~qg, 
                                                     data = filter(dhat, .data$ug == ugg)))
    dhat$discount <- do.call(base::c, lapply(loess_model, stats::predict))
  }
  dhat$yield <- -log(dhat$discount) / dhat$qg
  dhat <- dplyr::rename(dhat,
                        xgrid = "ug",
                        tau = "qg"
  )
  return(dhat)
}
