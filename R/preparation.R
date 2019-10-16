#' @name epaker
#' @title epaker kernel function
#' @description Kernel function for grid windows
#' @param x value to apply kernel to
#' @author Bonsoo Koo and Kai-Yang Goh
#' @keywords internal
epaker <- function(x) {
  (3/4)*(1-x^2)*(abs(x)<=1)
  # (1-x^2)*(abs(x)<=1)
}

#' @name calc_price_slist
#' @description Tranforms data into a format suitable for estimation
#' @aliases calc_price_slist
#' @title Create a bond price list from the data
#' @export
#' @param data a bond data
#' @return price_slist
#' @examples
#' \dontrun{
#' calc_price_slist(USbonds)
#' }
#' @author Bonsoo Koo and Kai-Yang Goh
#' @details
#' This function converts and extracts bond prices over
#' quotation days and different bonds from the raw data
#' into a list of sparse matrices for estimation
calc_price_slist <- function(data) {
  price_list <- data %>%
    mutate(mid.price = !!sym('mid.price') + !!sym('accint')) %>%
    select(!!sym('qdate'), !!sym('crspid'), !!sym('tupq'), !!sym('mid.price')) %>%
    group_by(!!sym('qdate'), !!sym('crspid'))
  price_list <- split(price_list, price_list$qdate)

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
  names(price_slist) <- names(price_list)
  return(price_slist)
}


#' @name calc_cf_slist
#' @aliases calc_cf_slist
#' @description Tranforms data into a format suitable for estimation
#' @title Create a list of sparse matrices to represent cash flows
#' @export
#' @param data data for bonds including quotation days, bond id,
#' time until payment and payment amount.
#' @return If all the required information is present, then the
#' output will be a list with length equal to the number of quotation
#' days in the data. Each element of the output will be a sparseMatrix
#' object.
#' @examples
#' \dontrun{
#' calc_cf_slist(USbonds)
#' }
#' @author Bonsoo Koo and Kai-Yang Goh
#' @details
#' This function converts and extracts coupon payments over
#' quotation days and different bonds from the raw data
#' into a list of sparse matrices for estimation
calc_cf_slist <- function(data) {

  cf_list <- data %>%
    select(!!sym('qdate'), !!sym('crspid'), !!sym('tupq'), !!sym('pdint')) %>%
    group_by(!!sym('qdate'), !!sym('crspid'))
  cf_list <- split(cf_list, cf_list$qdate)

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
  names(cf_slist) <- names(cf_list)
  return(cf_slist)
}


#' @name calc_uu_window
#' @aliases calc_uu_window
#' @title Weights time grid
#' @description Generate a kernel weight function in relation to time grids
#' @export
#' @param data a bond dataframe
#' @param ugrid vector of the quotation date grid
#' @param hu vector of quotation date bandwidth
#' @return calc_uu_window
#' @keywords internal
#' @author Bonsoo Koo and Kai-Yang Goh
#' @details
#' This function generates a weight function attached to each quotation date grid
#' for the estimation of a discount function
calc_uu_window <- function(data, ugrid, hu) {
  #u = values you want to compute dbar and hhat for
  #h = bandwidth parameter
  qdate_len <- length(unique(data$qdate))
  gamma <- seq(1, qdate_len, 1) / qdate_len
  uu = matrix(0, nrow = length(gamma), ncol = length(ugrid))
  for (i in 1:length(ugrid)) {
    uu[,i] <- sapply(ugrid[i], function(y) (y - gamma)/hu[i])
  }
  epaker(uu)
}

#' @name calc_r_window
#' @aliases calc_r_window
#' @title Weights interest rate grid
#' @description Generate a kernel weight function in relation to interest rate grids
#' @export
#' @param interest vector of daily interest rates
#' @param rgrid vector of interest rate grid
#' @param hr vector of intereest rate bandwidth
#' @return calc_r_window
#' @keywords internal
#' @author Bonsoo Koo, Kai-Yang Goh and Nathaniel Tomasetti
#' @details
#' This function generates a weight function attached to each interest rate grid
#' for the estimation of a discount function
calc_r_window <- function(interest, rgrid, hr) {

  r <- matrix(0, nrow = length(interest), ncol = length(rgrid))
  for (i in 1:length(rgrid)) {
    r[,i] <- sapply(rgrid[i], function(y) (y - interest)/hr[i])
  }
  epaker(r)
}

#' @name calc_day_idx
#' @aliases calc_day_idx
#' @title Provide indices in relation to time grids
#' @param data a bond data frame
#' @param ugrid vector of quotation date grid
#' @param hu vector of quotation date bandwidth
#' @return calc_day_idx
#' @keywords internal
#' @author Bonsoo Koo and Kai-Yang Goh
#' @export
#' @details
#' This function provide indices for the start and end of the qdates included in the ugrid kernel windows
calc_day_idx <- function(data, ugrid, hu) {
  u <- calc_uu_window(data, ugrid, hu)
  apply(u, 2, function(y) {
    window_idx <- which(y != 0)
    return(c(window_idx[1], window_idx[length(window_idx)]))
  }) %>%
    t()
}


#' @name calc_ux_window
#' @aliases calc_ux_window
#' @export
#' @title Weigts time to maturity grid
#' @description Apply kernel in relation to time-to-maturity grids
#' @param data a bond data frame
#' @param xgrid vector of the time-to-maturity grid
#' @param hx vector of the time-to-maturity grid bandwidth
#' @return calc_ux_window
#' @keywords internal
#' @author Bonsoo Koo and Kai-Yang Goh
#' @details
#' This function generates a weight function attached to each time grid
#' for the estimation of a discount function
calc_ux_window <- function(data, xgrid, hx, units = 365) {
  #x = values you want to compute dbar and hhat for
  #h = bandwidth parameter
  tupq_len<- as.integer(max(data$tupq))
  tau <- seq(1, as.integer(tupq_len), 1)/ units
  len_xgrid <- length(xgrid)
  ux <- sapply(1:len_xgrid, function(y) (xgrid[y] - tau)/hx[y])
  epaker(ux)
}

#' @name calc_tupq_idx
#' @aliases calc_tupq_idx
#' @title Provide indices in relation to time-to-maturity grids
#' @export
#' @param data Bond dataframe
#' @param xgrid vector of the time-to-maturity grid
#' @param hx vector of the time-to-maturity grid bandwidth
#' @return calc_tupq_idx
#' @keywords internal
#' @author Bonsoo Koo and Kai-Yang Goh
#' @details
#' This function provides indices for the first and last xgrid included in the current kernel window
calc_tupq_idx <- function(data, xgrid, hx, units = 365) {
  x <- calc_ux_window(data,xgrid,hx, units)
  apply(x, 2, function(y) {
    window_idx <- which(y > 0.01)
    return(c(window_idx[1], window_idx[length(window_idx)]))
  }) %>%
    t()
}

#' @name create_xgrid_hx
#' @title Automatic selection of xgrid and hx values
#' @description Selects xgrid and hx from values of qgrid and hq with a given number of maturing bonds
#' @details Automatically select values for sparse time to maturity xgrid and hx
#' for a given dense time to maturity value of qgrid and hq and quotation date ugrid and hu.
#' The length of the provided xgrid may change for different ugrid values, so it is reccomended
#' that the function is called separately for different values of ugrid.
#' @export
#' @author Nathaniel Tomasetti
#' @param data Bond dataframe
#' @param ugrid A single value for ugrid between 0 and 1
#' @param hu A single value for the bandwidth of the ugrid value
#' @param qgrid vector of dense time to maturity grid
#' @param hq vector of dense time to maturity bandwidth
#' @param min_points Integer, minimum number of maturing bonds in a qgrid range to be included in xgrid.
#' @param rgrid Optional, a single value for rgrid
#' @param hr Optional, A single value for the bandwidth of the rgrid value
#' @param interest, Optional, a vector of daily interest rates for use with rgrid. Must have a length equal to the number of unique qdates in data
#' @param units, Optional, number of units per period. Eg 365 for daily data, 12 for monthly.
#' Grid values without maturing bonds do not have sufficient data for stable estimates.
create_xgrid_hx <- function(data, ugrid, hu, qgrid, hq, min_points, rgrid, hr, interest, units = 365){

  points <- num_points_mat(data, ugrid, hu, qgrid, hq, rgrid, hr, interest, units)


  # Shrink window if there is not enough bonds at the end
  while(points[length(points)] < min_points){
    points <- points[1:(length(points)-1)]
    hq <- hq[1:length(points)]
    qgrid <- qgrid[1:length(points)]
  }

  # Set hx to hq, but exclude points where there are not many bonds maturing
  hx <- hq
  hx[points < min_points] <- NA
  # Find where the gaps start: the indices of elements of hx that are non na, but are followed by an na, and where the gap ends: a non-na hx that follows an na hx
  gap_start <- which(!is.na(hx) & dplyr::lead(is.na(hx)))
  gap_end <- which(!is.na(hx) & dplyr::lag(is.na(hx)))
  # Amount of time in each gap
  gap_size <- qgrid[gap_end] - qgrid[gap_start]
  # hx at the edges of missing values are extended so half the gap is covered by each end
  hx[gap_start] <- gap_size / 2
  hx[gap_end] <- gap_size / 2
  # Remove NA values
  hx <- hx[!is.na(hx)]
  # Set xgrid to be qgrid without the area without much data
  xgrid <- qgrid[points >= min_points]
  list(xgrid = xgrid, hx = hx, qgrid = qgrid, hq = hq)
}

#' @name interpolate_discount
#' @title Interpolates the discount rates resuling from estimate_yield
#' @description Applies three dimensional interpolation to find discount rates suitable for provided data
#' @details Given a bond data object and the output of estimate_yield, this function interpolates over each grid
#' to obtain new values of the discount rate for arbitary rgrid / ugrid and qgrid values.
#' @author Nathaniel Tomasetti
#' @param data Bond datafrmae
#' @param yield Output of estimate_yield
#' @param treasury Optional, dataframe of daily treasury bill rates with date and rate columns
#' @importFrom rlang .data
#' @importFrom dplyr ungroup distinct
#' @importFrom stats loess predict
#' @export
interpolate_discount <- function(data, yield, treasury){

  # Check dates included in data
  dates <- unique(data$qdate)
  # Check if rgrid was included, add a dummy column if required
  if(is.null(yield$rg)){
    rg_included <- FALSE
    yield$rg <- 0
  } else {
    rg_included <- TRUE
    if(missing(treasury)){
      stop('treasury argument must be included when rgrid appears in the estimate_yield output')
    }
    if(!all(dates %in% treasury$date)){
      stop('treasury argument does not include dates appearing in data object')
    }
  }

  # Find unique values of xg (time to payment) in the data. Exclude time to payments before first qgrid value as this doesn't work well with the loess interpolator
  data %>%
    ungroup() %>%
    filter(.data$tupq > min(yield$qg) * 365) %>%
    mutate(x = as.numeric(.data$tupq) / 365) %>%
    select(.data$x) %>%
    distinct() %>%
    .$x %>%
    sort() -> long_term_x

  # Linear interpolation for those values we excluded earlier
  data %>%
    ungroup() %>%
    filter(.data$tupq <= min(yield$qg) * 365)%>%
    mutate(x = as.numeric(.data$tupq) / 365) %>%
    select(.data$x) %>%
    distinct() %>%
    .$x %>%
    sort() -> short_term_x

  unique_x <- c(short_term_x, long_term_x)

  # Extract ugrid and rgrid
  yield %>%
    select(.data$ug) %>%
    unique() %>%
    .$ug -> ugrid
  yield %>%
    select(.data$ug, .data$rg) %>%
    unique() %>%
    .$rg %>%
    matrix(ncol = length(.data$ugrid)) %>%
    t() -> rgrid

  # Loess interpolation of each ugrid / rgrid combination for these qgrid values
  interp <- array(0, dim = c(length(unique_x), length(ugrid), ncol(rgrid)))
  for(i in 1:length(ugrid)){
    for(j in 1:ncol(rgrid)){
      res <- filter(yield, .data$ug == ugrid[i] & .data$rg == rgrid[i, j])
      loess_fit <- loess(discount ~ qg, data = res)
      interp[(length(short_term_x)+1):length(unique_x), i, j] <- predict(loess_fit, long_term_x)


      # Linear interpolation for the very small x values, where dhat = 1 at x = 0.
      min_res <- filter(res, .data$qg == min(.data$qg))
      interp[1:length(short_term_x), i, j] <- 1 + (short_term_x - 0) * (min_res$discount - 1) / (min_res$qg - 0)

    }
  }

  # Linear interpolation over ugrid and/or rgrid
  if(rg_included){

    data %>%
      ungroup() %>%
      mutate(x = as.numeric(.data$tupq) / 365,
             qdateF = factor(.data$qdate, labels = .data$dates),
             u = as.numeric(.data$qdateF) / length(.data$dates)) %>%
      left_join(treasury %>% select(.data$date, .data$rate),
                by = c('qdate'= 'date')) -> data

    data %>%
      group_by(.data$qdate, .data$crspid, .data$matdate, .data$mid.price, .data$accint, .data$x, .data$type) %>%
      mutate(discount = as.numeric((interpolate_ugrid_rgrid(.data$x, .data$u, .data$rate, ugrid, rgrid, interp, unique_x)))) %>%
      ungroup() -> data

  } else {

    data %>%
      ungroup() %>%
      mutate(x = as.numeric(.data$tupq) / 365,
             qdateF = factor(.data$qdate, labels = .data$dates),
             u = as.numeric(.data$qdateF) / length(.data$dates)) -> data

    data %>%
      group_by(.data$qdate, .data$crspid, .data$matdate, .data$mid.price, .data$accint, .data$x, .data$type) %>%
      mutate(discount = as.numeric((interpolate_ugrid(.data$x, .data$u, ugrid, interp[,,1], unique_x)))) %>%
      ungroup() -> data

    data$rg <- NULL

  }

  data
}


