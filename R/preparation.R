

# Create lists of sparse matrices to represent cash flows and price
#
# Tranforms data into a format suitable for estimation
#
# This function converts and extracts coupon payments and bond prices over
# quotation days and different bonds from the raw data
# into a list of sparse matrices for estimation
#
# @inheritParams estimate_yield
# @return If all the required information is present, then the
# output will be two lists, each with length equal to the number of quotation
# days in the data. In each list, each element of the output will be a sparseMatrix
# object for a quotation day with the number of rows being the number of bonds
# and the number of columns being the maximum of time-to-maturity.
# Each row is a bond with cash flow or bond price indicated at the corresponding column (time).
# @examples
# \donttest{
# cfp_slist <- get_cfp_slist(ycevo_data())
# }
get_cfp_slist <- function(data){
  cfp_list <- data %>%
    select("qdate", "id", "tupq", "pdint", "price") %>%
    split(.$qdate)
  id <- unique(data$id)
  id_len <- length(id)
  tupq_len <- as.integer(max(data$tupq))
  qdate_len <- length(unique(data$qdate))
  price_slist <- vector(mode = "list", length = qdate_len)
  cf_slist <- vector(mode = "list", length = qdate_len)
  seq_tupq <- 1:tupq_len

  for (i in 1:qdate_len) {
    x_list <- cfp_list[[i]]

    price_slist[[i]] <- sparseMatrix(i = match(x_list$id, id),
                                     j = match(x_list$tupq, seq_tupq),
                                     x = x_list$price,
                                     dims = c(id_len, tupq_len),
                                     dimnames=list(id,seq_tupq))
    cf_slist[[i]] <- sparseMatrix(i = match(x_list$id, id),
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

# Calculate Epanechnikov kernel weights at `gamma`
# surrounding point `grid`` with `bandwidth`
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
#  out <- get_weights(grid = xgrid, bandwidth = hx, len = length(unique(ycevo_data()$qdate)))
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- get_weights(grid = tau, bandwidth = ht,
#                    len = as.integer(max(ycevo_data()$tupq)),
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
#                           len = length(unique(ycevo_data()$qdate))))
# # Time to maturity
# tau <- c(30, 60, 90) / 365
# ht <- c(15, 15 , 15) / 365
# out <- range_idx_nonzero(
#   get_weights(
#     grid = tau, bandwidth = ht,
#     len = as.integer(max(ycevo_data()$tupq)),
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



# Automatic selection of tau and ht values
#
# Selects tau and ht from values of tau_p and htp with a given number of maturing bonds
#
# Automatically select values for sparse time to maturity \code{tau}, \code{tau_p}, \code{ht}, \code{htp}
# for a given quotation date xgrid and hx.
# Grid values without maturing bonds do not have sufficient data for stable estimates.
# Grid values at the end of the sequence that do not cover enough bonds are dropped.
# Values of \code{tau} that do not cover enough bonds in the middle of the sequence will be dropped,
# and bandwidth \code{ht} associated with the two ends of the gap will be extended to cover the gap in the middle.
# will be extended to cover the
#
# The length of the provided tau may change for different xgrid values, so it is recommended
# that the function is called separately for different values of xgrid.
# @inheritParams ycevo
# @param xgrid A single value for xgrid between 0 and 1
# @param hx A single value for the bandwidth of the xgrid value
# @param min_points Integer, minimum number of maturing bonds in a tau_p range to be included in tau.
# @param rgrid Optional, a single value for rgrid
# @param hr Optional, A single value for the bandwidth of the rgrid value
# @return List of \code{tau}, \code{ht}, \code{tau_p} and \code{htp}.
# @examples
#  xgrid <- 0.2
#  hx <- 0.18
#  tau_p <- c(30, 60, 90) / 365
#  htp <- c(15, 15 , 15) / 365
#  out <- create_tau_ht(data = ycevo_data(), xgrid = xgrid, hx = hx, tau_p = tau_p,htp =htp, min_points = 5)
create_tau_ht <- function(xgrid, hx, ht, htp, min_points = 5, data, tau, tau_p, rgrid, hr, interest) {
  ht <- as.numeric(ht)
  htp <- as.numeric(htp)
  ## tau and ht
  points <- num_points_mat(data, xgrid, hx, tau, ht, rgrid, hr, interest)

  # Shrink window if there is not enough bonds at the end
  while(points[length(points)] < min_points){
    points <- points[seq_len(length(points)-1)]
  }
  tau <- tau[seq_along(points)]
  ht <- ht[seq_along(points)]

  ht_narrow <- points < min_points

  # Find where the gaps start: the indices of elements of ht that are non na,
  # but are followed by an na, and where the gap ends: a non-na ht that follows an na ht
  gap_start <- which(!ht_narrow & dplyr::lead(ht_narrow))
  gap_end <- which(!ht_narrow & dplyr::lag(ht_narrow))
  # Amount of time in each gap
  gap_size <- tau[gap_end] - tau[gap_start]
  # ht at the edges of missing values are extended so half the gap is covered by each end
  ht[gap_start] <- gap_size / 2
  ht[gap_end] <- gap_size / 2
  # Remove NA values
  ht <- ht[!ht_narrow]
  tau <- tau[!ht_narrow]

  ## tau_p and htp
  points <- num_points_mat(data, xgrid, hx, tau_p, htp, rgrid, hr, interest)
  # Shrink window if there is not enough bonds at the end
  while(points[length(points)] < min_points){
    points <- points[seq_len(length(points)-1)]
  }
  tau_p <- tau_p[seq_along(points)]
  htp <- htp[seq_along(points)]

  list(tau = tau, ht = ht, tau_p = tau_p, htp = htp)

}