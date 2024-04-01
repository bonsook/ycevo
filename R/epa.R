# Epanechnikov kernel function
#
# Kernel function for grid windows
epaker <- function(x) {
  (3/4)*(1-x^2)*(abs(x)<=1)
}

qepa <- function(p, mu, r) {
  2 * sin(asin(2 * p - 1)/3) * r + mu
}

repa <- function(n, mu, r) {
  qepa(runif(n, min = 0, max = 1), mu, r)
}


#' Visualising the span and weight from the kernel function 
#' 
#' @param data Bond data. If \code{x} and \code{hx} are not NULL, \code{data} needs to include one 
#' time index column \code{qdate}. If \code{tau} and \code{ht} are not NULL, \code{data} needs to include one 
#' column of time to maturity in days \code{tupq}.
#' @importFrom ggplot2 geom_hline scale_y_continuous expansion xlab
#' @export
vis_kernel <- function(data, 
                       x = NULL, hx = NULL, 
                       tau = NULL, ht = NULL, ...) {
  p <- NULL
  covar_ls <- handle_covariates(data, ...)
  
  if(assert_same_nullness(x, hx)) {
    assert_same_length(x, hx)
    if((!is.null(tau)) || (!is.null(ht))) {
      stop("If \"x\" and \"hx\" are specified, \"tau\" and \"ht\" cannot be specified.")
    }
    if(!is.null(covar_ls$dots_name)) {
      stop("If \"x\" and \"hx\" are specified, \"...\" cannot be specified.")
    }
    dates <- sort(unique(data$qdate))
    ndates <- length(dates)
    nx <- length(x)
    xgrid <- stats::ecdf(dates)(x)
    nhx <- length(hx)
    gamma <- seq_along(dates)/ndates
    
    df_p <- data.frame(dates = rep(dates, max(nhx, nx)), 
                       gamma = rep(gamma, max(nhx, nx)), 
                       xgrid = rep(xgrid, each = ndates), 
                       hx = rep(hx, each = ndates)) %>% 
      mutate(weight = epaker((gamma - xgrid)/hx)) %>% 
      mutate(hx = as.factor(hx)) 
    
    p <- df_p %>% 
      ggplot(aes(x = .data$dates, y= .data$weight)) +
      geom_line(aes(colour = hx, group = interaction(hx, xgrid))) +
      geom_hline(yintercept = 0, linewidth = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    
  } 
  
  if(assert_same_nullness(tau, ht)) {
    assert_same_length(tau, ht)
    if(!is.null(covar_ls$dots_name)) {
      stop("If \"tau\" and \"ht\" are specified, \"...\" cannot be specified.")
    }
    ntau <- length(tau)
    nht <- length(ht)
    gamma <- seq_len(as.integer(max(data$tupq)))/365
    ngamma <- length(gamma)
    
    df_p <- data.frame(
      gamma = rep(gamma, max(nht, ntau)), 
      tau = rep(tau, each = ngamma), 
      ht = rep(ht, each = ngamma)) %>% 
      mutate(weight = epaker((tau - gamma)/ht)) %>% 
      mutate(ht = as.factor(ht)) 
    
    p <- df_p %>% 
      ggplot(aes(x = .data$gamma, y= .data$weight)) +
      geom_line(aes(colour = ht, group = interaction(ht, tau))) +
      xlab("tau") +
      geom_hline(yintercept = 0, linewidth = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  }  
    
  if(!is.null(covar_ls$dots_name)) {
    interest <- covar_ls$interest
    rgrid <- covar_ls$rgrid
    hr <- covar_ls$hr
    dot_name <- covar_ls$dots_name
    
    nrgrid <- length(rgrid)
    nhr <- length(hr)
    gamma <- interest
    ngamma <- length(gamma)
    calc_epaker_weights(interest, rgrid, hr)
    
    df_p <- data.frame(
      gamma = rep(gamma, max(nhr, nrgrid)), 
      rgrid = rep(rgrid, each = ngamma), 
      hr = rep(hr, each = ngamma)) %>% 
      mutate(weight = epaker((rgrid - gamma)/hr)) %>% 
      mutate(hr = as.factor(hr)) 
    p <- df_p %>% 
      ggplot(aes(x = .data$gamma, y= .data$weight)) +
      geom_line(aes(colour = hr, group = interaction(hr, rgrid))) +
      xlab(dot_name) +
      geom_hline(yintercept = 0, linewidth = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    
  }
  
  p
}

