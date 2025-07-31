#' Estimate yield function
#'
#' @md
#' @description
#'
#'   Nonparametric estimation of discount functions and yield curves at given
#'   dates, time-to-maturities, and one additional covariate, usually interest
#'   rate.
#'
#' @details Suppose that a bond \eqn{i} has a price \eqn{p_i} at time \eqn{t} with a
#'   set of cash payments, say \eqn{c_1, c_2, \ldots, c_m} with a set of
#'   corresponding discount values \eqn{d_1, d_2, \ldots, d_m}. In the bond
#'   pricing literature, the market price of a bond should reflect the
#'   discounted value of cash payments. Thus, we want to minimise
#' \deqn{(p_i-\sum^m_{j=1}c_j\times d_j)^2.} For the estimation of \eqn{d_k(k=1,
#' \ldots, m)}, solving the first order condition yields
#' \deqn{(p_i-\sum^m_{j=1}c_j \times d_j)c_k = 0, } and \deqn{\hat{d}_k =
#' \frac{p_i c_k}{c_k^2} - \frac{\sum^m_{j=1,k\neq k}c_k c_j d_j}{c_k^2}.}
#'
#'   There are challenges: \eqn{\hat{d}_k} depends on all the relevant discount
#'   values for the cash payments of the bond. Our model contains random errors
#'   and our interest lies in expected value of \eqn{d(.)} where the expected
#'   value of errors is zero. \eqn{d(.)} is an infinite-dimensional function not
#'   a discrete finite-dimensional vector. Generally, cash payments are made
#'   biannually, not dense at all. Moreover, cash payment schedules vary over
#'   different bonds.
#'
#'   Let \eqn{d(\tau, X_t)} be the discount function at given covariates
#'   \eqn{X_t} (dates `x` and interest rates `rgrid`), and given
#'   time-to-maturities \eqn{\tau} (`tau`). \eqn{y(\tau, X_t)} is the yield
#'   curve at given covariates \eqn{X_t} (dates `x` and interest rates
#'   `rgrid`), and given time-to-maturities \eqn{\tau} (`tau`).
#'
#'   We pursue the minimum of the following smoothed sample least squares
#' objective function for any smooth function \eqn{d(.)}: \deqn{Q(d) =
#' \sum^T_{t=1}\sum^n_{i=1}\int\{p_{it}-\sum^{m_{it}}_{j=1}c_{it}(\tau_{ij})d(s_{ij},
#' x)\}^2 \sum^{m_{it}}_{k=1}\{K_h(s_{ik}-\tau_{ik})ds_{ik}\}K_h(x-X_t)dx,}
#'   where a bond \eqn{i} has a price \eqn{p_i} at time \eqn{t} with a set of cash
#'   payments \eqn{c_1, c_2, \ldots, c_m} with a set of corresponding discount
#'   values \eqn{d_1, d_2, \ldots, d_m}, \eqn{K_h(.) = K(./h)} is the kernel
#'   function with a bandwidth parameter \eqn{h}, the first kernel function is
#'   the kernel in space with bonds whose maturities \eqn{s_{ik}} are close to
#'   the sequence \eqn{\tau_{ik}}, the second kernel function is the kernel in
#'   time and in interest rates with \eqn{x}, which are close to the sequence
#'   \eqn{X_t}. This means that bonds with similar cash flows, and traded in
#'   contiguous days, where the short term interest rates in the market are
#'   similar, are combined for the estimation of the discount function at a
#'   point in space, in time, and in "interest rates".
#'
#'   The estimator for the discount function over time to maturity and time is
#'   \deqn{\hat{d}=\arg\min_d Q(d).} This function provides a data frame of the
#'   estimated yield and discount rate at each combination of the provided
#'   grids. The estimated yield is transformed from the estimated discount rate.
#'
#'   An alternative specification of bandwidth `hx` is `span_x`, which provides
#'   kernel coverage invariant to the length of `data`. `span_x` takes an
#'   absolute measure of time depending on the unit of `x`. The default value is
#'   60. If the data is daily on trading days, i.e., the interval between every
#'   two consecutive `qdate` is one trading day, then the window of the kernel
#'   function allows the estimation at each point `x` to contain information
#'   from 60 trading days prior to and after the time point `x`.
#'
#'   For more information on the estimation method, please refer to
#'   `References`.
#'
#' @param data Data frame; bond data to estimate discount curve from. See
#'   [ycevo_data()] for an example bond data structure. Minimum required columns
#'   are `qdate`, `id`, `price`, `tupq`, and `pdint`. The columns can be named
#'   differently: see `cols`.
#' @param x Time grids at which the discount curve is evaluated. Should be
#'   specified using the same class of object as the quotation date (`qdate`)
#'   column in `data`.
#' @param span_x Half of the window size, or the distance from the centre `x` to
#'   the maximum (or the minimum) `qdate` with non-zero weight using the kernel
#'   function, measured by the number of regular interval between two
#'   consecutive `qdate`. Ignored if `hx` is specified. See `Details`.
#' @param hx Numeric vector. Bandwidth parameters corresponding to each time
#'   point `x`.
#' @param tau Numeric vector. Time-to-maturities in years where
#'   discount function and yield curve will be estimated for each of time points `x`.
#'   See `Details`.
#' @param ht Numeric vector. Bandwidth parameters corresponding to each value of
#'   time-to-maturities `tau`. See `Details`.
#' @param tau_p Numeric vector. Auxiliary time-to-maturities in
#'   years. See `Details`.
#' @param htp Numeric vector. Bandwidth parameters corresponding to each of
#'   auxiliary time-to-maturities `tau_p`. See `Details`.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> A named list or vector of
#'   alternative names of required variables, following the `new_name =
#'   old_name` syntax of the [dplyr::rename()], where the `new_nam` takes one of
#'   the five column names required in `data`. This enables the user to provide
#'   `data` with columns named differently from required.
#' @param ... Specification of an additional covariate, taking the form of `var
#'   = list(grid, bandwidth)`, where `var` is the name of the covariate in
#'   `data`, `grid` is the values at which the yield curve is estimated,
#'   similar to `x`, and `bandwidth` is the bandwidth parameter corresponding to
#'   each of the `grid` values, similar to `hx`.
#'
#' @returns A [tibble::tibble()] object of class `ycevo` with the following
#'   columns.
#'
#'   \describe{
#'     \item{qdate}{The time points that user-specified as `x`. The name of this
#'       column will be consistent with the name of the time index column in the
#'       `data` input, if the user choose to provide a data frame with the time
#'       index column named differently from `qdate` with the `cols` argument.}
#'     \item{.est}{A nested columns of estimation results containing a
#'       [tibble::tibble()] for each `qdate`. Each `tibble` contains three columns:
#'       `tau` for the time-to-maturity specified by the user in the `tau` argument,
#'       `.disount` for the estimated discount function at this time and this
#'       time-to-maturity, and `.yield` for the estimated yield curve.}
#'   }
#' @seealso [augment.ycevo()], [autoplot.ycevo()]
#' @examples
#' # Simulating bond data
#' bonds <- ycevo_data(n = 10)
#' \donttest{
#' # Estimation can take up to 30 seconds
#' ycevo(bonds, x = lubridate::ymd("2023-03-01"))
#' }
#'
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a
#'   nonparametric model for bond prices from cross-section and time series
#'   information. Journal of Econometrics, 220(2), 562-588.
#' @order 1
#' @export
ycevo <- function(
  data,
  x,
  span_x = 60,
  hx = NULL,
  tau = NULL,
  ht = NULL,
  tau_p = tau,
  htp = NULL,
  cols = NULL,
  ...
) {
  assert_class(data, "data.frame")
  assert_no_missing(x)
  assert_no_missing(tau)
  assert_unique(x)
  assert_unique(tau)

  # Now use id, not crspid
  if (any(colnames(data) == "crspid")) {
    warning(
      'Column name "crspid" is deprecated. Column "id" is now used as asset identifier.'
    )
  }

  # The minimum required columns
  d_col <- c("qdate", "id", "price", "pdint", "tupq")
  qdate_label <- "qdate"

  # Handle cols renaming
  handled_cols <- handle_cols(data, enexpr(cols), d_col, qdate_label)
  data <- handled_cols$data
  qdate_label <- handled_cols$qdate_label

  # Handle interest rate
  covar_ls <- handle_covariates(data, ...)
  interest <- covar_ls$interest
  rgrid <- covar_ls$rgrid
  hr <- covar_ls$hr
  dot_name <- covar_ls$dots_name

  # Minimal data
  data <- select(data, all_of(d_col))

  xgrid <- stats::ecdf(data$qdate)(x)

  # Handle grids
  assert_length(span_x, len = c(1, length(x)))
  # xgrid and hx
  if (is.null(hx)) {
    hx <- vapply(
      span_x,
      function(span_x) span2h(span_x, length(unique(data$qdate))),
      FUN.VALUE = numeric(1)
    )
  }
  assert_length(hx, len = c(1, length(x)))

  if (length(hx) == 1) {
    hx <- rep(hx, length(xgrid))
  }

  # tau
  if (is.null(tau)) {
    max_tupq <- max(data$tupq)
    tau <- seq_tau(max_tupq / 365)
  }
  # ht
  if (is.null(ht)) {
    ht <- find_bandwidth_from_tau(tau)
  }
  # ht
  if (is.null(htp)) {
    htp <- find_bandwidth_from_tau(tau_p)
  }

  assert_length(ht, len = c(1, length(tau)))
  assert_length(htp, len = c(1, length(tau_p)))

  if (is.vector(ht)) {
    ht <- matrix(ht, nrow = length(ht), ncol = length(xgrid))
  }
  if (is.vector(htp)) {
    htp <- matrix(htp, nrow = length(htp), ncol = length(xgrid))
  }

  # sort grids
  # in case the user don't specify them in sorted order
  # xgrid
  order_xgrid <- order(xgrid)
  xgrid <- xgrid[order_xgrid]
  hx <- hx[order_xgrid]
  # tau
  order_tau <- order(tau)
  tau <- tau[order_tau]
  ht <- as.matrix(ht)[order_tau, order_xgrid, drop = FALSE]
  # tau_p
  order_tau_p <- order(tau_p)
  tau_p <- tau[order_tau_p]
  htp <- as.matrix(htp)[order_tau_p, order_xgrid, drop = FALSE]
  # rgrid
  if (!is.null(rgrid)) {
    rgrid_order <- order(rgrid)
    rgrid <- rgrid[rgrid_order]
    hr <- hr[rgrid_order]
  }

  # Handle tau and tau_p again
  # based on number of bonds in each window
  tau_adjusted <- mapply(
    create_tau_ht,
    xgrid = xgrid,
    hx = hx,
    ht = asplit(ht, 2),
    htp = asplit(htp, 2),
    MoreArgs = list(
      data = data,
      tau = tau,
      tau_p = tau_p,
      rgrid = rgrid,
      hr = hr,
      interest = interest
    ),
    SIMPLIFY = FALSE
  )

  pb <- progressr::progressor(length(xgrid))
  output <- future.apply::future_lapply(
    seq_along(xgrid),
    function(i) {
      on.exit(pb())
      dplyr::rename(
        estimate_yield(
          data = data,
          xgrid = xgrid[[i]],
          hx = hx[[i]],
          tau = tau_adjusted[[i]]$tau,
          ht = tau_adjusted[[i]]$ht,
          tau_p = tau_adjusted[[i]]$tau_p,
          htp = tau_adjusted[[i]]$htp,
          rgrid = rgrid[[i]],
          hr = hr[[i]],
          interest = interest
        ),
        .discount = "discount",
        .yield = "yield"
      )
    },
    future.seed = TRUE
  )

  res <- output %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(any_of(c(
      "xgrid",
      "rgrid",
      "tau",
      ".discount",
      ".yield"
    ))) %>%
    tidyr::nest(.est = c("tau", ".discount", ".yield")) %>%
    rename_with(
      function(x) rep(dot_name %||% character(0), length(x)),
      any_of("rgrid")
    ) %>%
    mutate(!!sym(qdate_label) := x, .before = 1) %>%
    select(-xgrid)

  attr(res, "cols") <- cols
  attr(res, "qdate_label") <- qdate_label
  new_ycevo(res)
}

# Default sequence of tau
seq_tau <- function(max_tau) {
  tau <- c(
    seq(30, 6 * 30, 30), # Monthly up to six months
    seq(240, 2 * 365, 60), # Two months up to two years
    seq(720 + 90, 6 * 365, 90), # Three months up to six years
    seq(2160 + 120, 20 * 365, 120), # Four months up to 20 years
    #               seq(20 * 365 + 182, 30 * 365, 182)) / 365 # Six months up to 30 years
    seq(20 * 365 + 182, 30.6 * 365, 182)
  ) /
    365
  tau[tau < max_tau]
}

# Default bandwidth for tau and tau_p
# The larger distance from neighbours
find_bandwidth_from_tau <- function(tau) {
  laggap <- tau - dplyr::lag(tau)
  leadgap <- dplyr::lead(tau) - tau
  vapply(
    1:length(tau),
    function(x) max(laggap[x], leadgap[x], na.rm = TRUE),
    numeric(1L)
  )
}


new_ycevo <- function(x) {
  structure(x, class = c("ycevo", class(x)))
}
