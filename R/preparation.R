# Epanechnikov kernel function
#
# Kernel function for grid windows
#
# @param x value to apply kernel to
# @author Bonsoo Koo and Kai-Yang Goh
epaker <- function(x) {
  (3/4)*(1-x^2)*(abs(x)<=1)
  # (1-x^2)*(abs(x)<=1)
}


# Create lists of sparse matrices to represent cash flows and price

# Tranforms data into a format suitable for estimation
# 
# This function converts and extracts coupon payments and bond prices over
# quotation days and different bonds from the raw data
# into a list of sparse matrices for estimation
# 
# @param data data for bonds including quotation days, bond id,
# time until payment and payment amount. See \code{?USbonds} for an example data structure.
# 
# @return If all the required information is present, then the
# output will be two lists, each with length equal to the number of quotation
# days in the data. In each list, each element of the output will be a sparseMatrix
# object for a quotation day with the number of rows being the number of bonds 
# and the number of columns being the maximum of time-to-maturity.
# Each row is a bond with cash flow or bond price indicated at the corresponding column (time).
# @examples
# \donttest{
# cfp_slist <- get_cfp_slist(USbonds)
# }
# @author Yangzhuoran Fin Yang, Bonsoo Koo and Kai-Yang Goh
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
get_cfp_slist <- function(data){
  cfp_list <- data %>%
    mutate(mid.price = .data$mid.price) %>%
    select("qdate", "crspid", "tupq", "pdint", "mid.price") %>% 
    group_by(.data$qdate) %>% 
    group_split() 
  id <- unique(data$crspid)
  id_len <- length(id)
  tupq_len <- as.integer(max(data$tupq))
  qdate_len <- length(unique(data$qdate))
  price_slist <- vector(mode = "list", length = qdate_len)
  cf_slist <- vector(mode = "list", length = qdate_len)
  seq_tupq <- 1:tupq_len
  
  for (i in 1:qdate_len) {
    x_list <- cfp_list[[i]]
    
    price_slist[[i]] <- sparseMatrix(i = match(x_list$crspid, id),
                                     j = match(x_list$tupq, seq_tupq),
                                     x = x_list$mid.price,
                                     dims = c(id_len, tupq_len),
                                     dimnames=list(id,seq_tupq))
    cf_slist[[i]] <- sparseMatrix(i = match(x_list$crspid, id),
                                  j = match(x_list$tupq, seq_tupq),
                                  x = x_list$pdint,
                                  dims = c(id_len, tupq_len),
                                  dimnames=list(id,seq_tupq))
  }
  names(price_slist) <-  unique(data$qdate)
  names(cf_slist) <-  unique(data$qdate)
  
  
  list(price_slist = price_slist, 
       cf_slist = cf_slist)
}

calc_epaker_weights <- function(gamma, grid, bandwidth) {
  uu <- mapply(function(grid, bandwidth) {(grid-gamma)/bandwidth}, 
               grid = grid, 
               bandwidth = bandwidth)
  
  epaker(uu)
}

# Weights grid
# 
# Generate kernel weights using Epaker kernal function for given grids
# 
# This function generates a weight function attached to each grid, 
# quotation time or time to maturity
# for the estimation of a discount function
# 
# @param grid vector of the  grid
# @param bandwidth vector of quotation date bandwidth
# @param len the length of the grid. 
# For quotation time, this is the number of unique quotation time
# For time to maturity, this is the maximum tupq
# 
# @return Matrix with number of columns being the length of \code{grid} and 
# number of rows being the number of unique grid.
# Each column represents the weights on each value in the sequence of length \code{len} 
# for that column of  \code{grid}.
# 
# @examples 
#  # quotation time
#  xgrid <- c(0.2,0.4)
#  hx <- c(0.18,0.18)
#  out <- get_weights(grid = xgrid, bandwidth = hx, len = length(unique(USbonds$qdate)))
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- get_weights(grid = tau, bandwidth = ht,
#                    len = as.integer(max(USbonds$tupq)),
#                    units = 365)
# # For time to maturity, units is 365 to match a year
get_weights <- function(grid, bandwidth, len, units = len) {
  #xgrid = values you want to compute dbar and hhat for
  #hx = bandwidth parameter
  gamma <- seq_len(len) / units
  as.matrix(calc_epaker_weights(gamma, grid, bandwidth))
}

# Range of indecies of nonzero weights
# 
# @param mat_weights matrix of weights (for example, from get_weights)
# @param threshold how close to 0 is 0? This sets a threshold.
# @return Matrix. The start and end of the index with nonzero entry of each column of the matrix
# 
# @examples 
# # quotation time
#  xgrid <- c(0.2,0.4)
#  hx <- c(0.18,0.18)
#  out <- range_idx_nonzero(get_weights(grid = xgrid, bandwidth = hx, 
#                           len = length(unique(USbonds$qdate))))
# # Time to maturity
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- range_idx_nonzero(
#   get_weights(
#     grid = tau, bandwidth = ht,
#     len = as.integer(max(USbonds$tupq)),
#     units = 365),
#   threshold = 0.01)
# # For time to maturity a small threshold is set.
range_idx_nonzero <- function(mat_weights, threshold = 0) {
  mat_notzero <- mat_weights > threshold
  t(apply(mat_notzero, 2, function(x) {
    idx <- which(x)
    if(length(idx) == 0) return(c(NA, NA))
    range(idx)
  }))
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
  
  calc_epaker_weights(interest, rgrid, hr)
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



