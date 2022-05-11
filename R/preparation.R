# Epaker kernel function
#
# Kernel function for grid windows
#
# @param x value to apply kernel to
# @author Bonsoo Koo and Kai-Yang Goh
epaker <- function(x) {
  (3/4)*(1-x^2)*(abs(x)<=1)
  # (1-x^2)*(abs(x)<=1)
}

# Create a bond price list from the data
# 
# Tranforms data into a format suitable for estimation
# 
# This function converts and extracts bond prices over
# quotation days and different bonds from the raw data
# into a list of sparse matrices for estimation
# 
# @param data a bond data. See \code{?USbonds} for an example data structure.
# 
# @return If all the required information is present, then the
# output will be a list with length equal to the number of quotation
# days in the data. Each element of the output will be a sparseMatrix
# object for a quotation day with the number of rows being the number of bonds 
# and the number of columns being the maximum of time-to-maturity.
# Each row is a bond with price indicated at the corresponding column (time).
# @examples
# \donttest{
# price <- calc_price_slist(USbonds)
# }
# @author Bonsoo Koo and Kai-Yang Goh
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
calc_price_slist <- function(data) {
  price_list <- data %>%
    mutate(mid.price = .data$mid.price + as.numeric(as.character(.data$accint))) %>%
    select(.data$qdate, .data$crspid, .data$tupq, .data$mid.price) 
  price_list <- price_list %>% 
    group_by(.data$qdate) %>% 
    group_split()
  
  id <- unique(data$crspid)
  id_len <- length(id)
  tupq_len <- as.integer(max(data$tupq))
  qdate_len <- length(unique(data$qdate))
  price_slist <- vector(mode = "list", length = qdate_len)
  seq_tupq <- 1:tupq_len
  
  for (i in 1:qdate_len) {
    x_list <- price_list[[i]]
    price_slist[[i]] <- sparseMatrix(i = match(x_list$crspid, id),
                                     j = match(x_list$tupq, seq_tupq),
                                     x = x_list$mid.price,
                                     dims = c(id_len, tupq_len),
                                     dimnames = list(id, seq_tupq))
  }
  names(price_slist) <-  unique(data$qdate)
  return(price_slist)
}


# Create a list of sparse matrices to represent cash flows
# 
# Tranforms data into a format suitable for estimation
# 
# This function converts and extracts coupon payments over
# quotation days and different bonds from the raw data
# into a list of sparse matrices for estimation
# 
# @param data data for bonds including quotation days, bond id,
# time until payment and payment amount.
# 
# @return If all the required information is present, then the
# output will be a list with length equal to the number of quotation
# days in the data. Each element of the output will be a sparseMatrix
# object for a quotation day with the number of rows being the number of bonds 
# and the number of columns being the maximum of time-to-maturity.
# Each row is a bond with cash flow indicated at the corresponding column (time).
# @examples
# \donttest{
# cf <- calc_cf_slist(USbonds)
# }
# @author Bonsoo Koo and Kai-Yang Goh
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
calc_cf_slist <- function(data) {
  
  cf_list <- select(data, .data$qdate, .data$crspid, .data$tupq, .data$pdint) 
  cf_list <- cf_list %>%
    group_by(.data$qdate) %>%
    group_split()

  id <- unique(data$crspid)
  id_len <- length(id)
  tupq_len <- as.integer(max(data$tupq))
  qdate_len <- length(unique(data$qdate))
  cf_slist <- vector(mode = "list", length = qdate_len)
  seq_tupq <- 1:tupq_len
  
  for (u in 1:qdate_len) {
    x_list <- cf_list[[u]]
    cf_slist[[u]] <- sparseMatrix(i = match(x_list$crspid, id),
                                  j = match(x_list$tupq, seq_tupq),
                                  x = x_list$pdint,
                                  dims = c(id_len, tupq_len)#)
                                  ,dimnames=list(id,seq_tupq))
  }
  names(cf_slist) <- unique(data$qdate)
  return(cf_slist)
}

calc_window_epaker <- function(gamma, grid, bandwidth) {
  uu <- mapply(function(grid, bandwidth) {(grid-gamma)/bandwidth}, 
               grid = grid, 
               bandwidth = bandwidth)
  
  epaker(uu)
}

# Weights time grid
# 
# Generate kernel weights using Epaker kernal function in relation to time grids
# 
# This function generates a weight function attached to each quotation date grid
# for the estimation of a discount function
# 
# @param data a bond dataframe
# @param xgrid vector of the quotation date grid
# @param hx vector of quotation date bandwidth
# 
# @return Matrix with number of columns being the length of \code{ugrid} and number of rows being the number of unique qdates.
# Each column represents the weights of each qdate for that \code{rgrid}.
# Each column is a \code{ugrid} date with the weights of the qdates used in discount function estimation. qdates correspond to rows.
# 
# @author Bonsoo Koo and Kai-Yang Goh
# @examples 
#  xgrid <- c(0.2,0.4)
#  hx <- c(0.18,0.18)
#  out <- calc_uu_window(data = USbonds, xgrid = xgrid,hx = hx)
calc_uu_window <- function(data, xgrid, hx) {
  #ugrid = values you want to compute dbar and hhat for
  #hu = bandwidth parameter
  qdate_len <- length(unique(data$qdate))
  gamma <- seq(1, qdate_len, 1) / qdate_len
  
  calc_window_epaker(gamma, xgrid, hx) %>% 
    as.matrix()
}

# Weights interest rate grid
# 
# Generate a kernel weight function in relation to interest rate grids
# 
# This function generates a weight function attached to each interest rate grid
# for the estimation of a discount function
# 
# @param interest vector of daily interest rates
# @param rgrid vector of interest rate grid
# @param hr vector of interest rate bandwidth
# 
# @return Matrix with number of columns being the length of \code{rgrid} and number of rows being the number of unique qdates.
# Each column represents the weights of each qdate for that \code{rgrid}.
# Each column is a \code{rgrid} date with the weights of the qdates used in discount function estimation. qdates correspond to rows.
# 
# @author Bonsoo Koo, Kai-Yang Goh and Nathaniel Tomasetti
# @examples 
#  interest <- c(1.01, 1.02)
#  rgrid <- c(0.2,0.4)
#  hr <- c(0.18,0.18)
#  out <- calc_r_window(interest = interest, rgrid = rgrid,hr = hr)
calc_r_window <- function(interest, rgrid, hr) {
  
  calc_window_epaker(interest, rgrid, hr)
}

# Provide indices in relation to time grids
# 
# This function provide indices for the start and end of the qdates included in the kernel windows for each \code{ugrid}.
# 
# @param data a bond data frame. See \code{?USbonds} for an example data structure.
# @param ugrid vector of quotation date grid
# @param hu vector of quotation date bandwidth
# 
# @return Matrix. The start and end of the qdates included in the ugrid kernel windows
# 
# @author Bonsoo Koo and Kai-Yang Goh
# @examples 
#  ugrid <- c(0.2,0.4)
#  hu <- c(0.18,0.18)
#  out <- calc_day_idx(data = USbonds, ugrid = ugrid, hu = hu)
calc_day_idx <- function(data, ugrid, hu) {
  u <- calc_uu_window(data, ugrid, hu)
  dim(u) <- c(length(unique(data$qdate)), length(ugrid))
  apply(u, 2, function(y) {
    window_idx <- which(y != 0)
    return(c(window_idx[1], window_idx[length(window_idx)]))
  }) %>%
    t()
}


# Weights time to maturity grid
# 
# Apply kernel in relation to time-to-maturity grids
# 
# This function generates a weight function attached to each time grid
# for the estimation of a discount function
# 
# @param data a bond data frame
# @param tau vector of the time-to-maturity grid
# @param ht vector of the time-to-maturity grid bandwidth
# 
# @return Matrix with number of columns being the length of \code{tau} and number of rows being the number of unique qdates.
# Each column represents the weights of each qdate for that \code{tau}.
# Each column is a \code{tau} date with the weights of the qdates used in discount function estimation. qdates correspond to rows.
# @author Bonsoo Koo and Kai-Yang Goh
# @examples 
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- calc_ux_window(data = USbonds, tau = tau, ht = ht)
calc_ux_window <- function(data, tau, ht, units = 365) {
  #x = values you want to compute dbar and hhat for
  #h = bandwidth parameter
  tupq_len<- as.integer(max(data$tupq))
  gamma <- seq_len(tupq_len)/ units
  
  calc_window_epaker(gamma, tau, ht)
}

# Provide indices in relation to time-to-maturity grids
# 
# This function provides indices for the first and last tau included in the kernel window of each \code{tau}.
# 
# @param data Bond dataframe
# @param tau vector of the time-to-maturity grid
# @param ht vector of the time-to-maturity grid bandwidth
# 
# @return Matrix. The first and last tau included in the current kernel window.
# 
# @author Bonsoo Koo and Kai-Yang Goh
# @examples 
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- calc_tupq_idx(data = USbonds, tau = tau, ht = ht)
calc_tupq_idx <- function(data, tau, ht, units = 365) {
  x <- calc_ux_window(data,tau,ht, units)
  apply(x, 2, function(y) {
    window_idx <- which(y > 0.01)
    lth <- length(window_idx)
    if(lth==0) lth <- 1
    return(c(window_idx[1], window_idx[lth]))
  }) %>%
    t()
}

# Automatic selection of tau and ht values
# 
# Selects tau and ht from values of tau_p and htp with a given number of maturing bonds
# 
# Automatically select values for sparse time to maturity \code{tau} and \code{ht}
# for a given dense time to maturity value of tau_p and htp and quotation date xgrid and hx.
# The length of the provided tau may change for different xgrid values, so it is recommended
# that the function is called separately for different values of xgrid.
# @param data Bond dataframe. See \code{?USbonds} for an example data strcture..
# @param xgrid A single value for xgrid between 0 and 1
# @param hx A single value for the bandwidth of the xgrid value
# @param tau_p vector of dense time to maturity grid
# @param htp vector of dense time to maturity bandwidth
# @param min_points Integer, minimum number of maturing bonds in a tau_p range to be included in tau.
# @param rgrid Optional, a single value for rgrid
# @param hr Optional, A single value for the bandwidth of the rgrid value
# @param interest, Optional, a vector of daily interest rates for use with rgrid. Must have a length equal to the number of unique qdates in data
# @param units, Optional, number of units per period. Eg 365 for daily data, 12 for monthly.
# Grid values without maturing bonds do not have sufficient data for stable estimates.
# @return List of \code{tau}, \code{ht}, \code{tau_p} and \code{htp}. \code{tau_p} and \code{htp} is the same as input. 
# For the usage of created \code{tau} and \code{ht}, see \code{\link{estimate_yield}}.
# @examples 
#  xgrid <- 0.2
#  hx <- 0.18
#  tau_p <- c(30, 60, 90) / 365
#  htp <- c(15, 15 , 15) / 365
#  out <- create_tau_ht(data = USbonds, xgrid = xgrid, hx = hx, tau_p = tau_p,htp =htp, min_points = 5)
# @author Nathaniel Tomasetti
create_xgrid_hx <- function(data, xgrid, hx, tau, ht, min_points, rgrid = NULL, hr = NULL, interest = NULL, units = 365){
  
  tau_p <- tau
  htp <- ht
  points <- num_points_mat(data, xgrid, hx, tau, ht, rgrid, hr, interest, units)
  
  
  # Shrink window if there is not enough bonds at the end
  while(points[length(points)] < min_points){
    points <- points[seq_len(length(points)-1)]
    htp <- ht[seq_along(points)]
    tau_p <- tau[seq_along(points)]
  }
  
  # Set ht to htp, but exclude points where there are not many bonds maturing
  ht <- htp
  ht[points < min_points] <- NA
  # Find where the gaps start: the indices of elements of ht that are non na, but are followed by an na, and where the gap ends: a non-na ht that follows an na ht
  gap_start <- which(!is.na(ht) & dplyr::lead(is.na(ht)))
  gap_end <- which(!is.na(ht) & dplyr::lag(is.na(ht)))
  # Amount of time in each gap
  gap_size <- tau_p[gap_end] - tau_p[gap_start]
  # ht at the edges of missing values are extended so half the gap is covered by each end
  ht[gap_start] <- gap_size / 2
  ht[gap_end] <- gap_size / 2
  # Remove NA values
  ht <- ht[!is.na(ht)]
  # Set tau to be tau_p without the area without much data
  tau <- tau_p[points >= min_points]
  list(tau = tau, ht = ht, tau_p = tau_p, htp = htp)
}



