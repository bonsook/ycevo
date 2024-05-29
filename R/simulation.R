#' Simulate bond data
#'
#' Simulates bond transaction data at each weekday throughout the year 2023,
#' following the extended version of Nelson & Siegel model (Nelson, C. R., &
#' Siegel, A. F., 1987).
#'
#' `n` bonds for each of the following maturities are simulated: 20, 10, 5,
#' 3, 2 and 0.8 years. The face value of all bonds is 100. Bonds with 0.8 years
#' of maturity are similar to the US Treasury bills with no coupon. Bonds with
#' maturity between 2 and 10 years correspond to the US Treasury notes. Their
#' coupon rates are simulated from an Epanechnikov distribution with mean 4, and
#' the distance from the mean to the boundary is 2.65. Bonds with maturity of 20
#' years corresponds to the US Treasury bonds. Their coupon rates are simulated
#' from an Epanechnikov distribution with mean 7.5, and the distance from the
#' mean to the boundary is 2.65. Coupons are payable every 6 months.
#'
#' We artificially "observe" quotation data of bonds on every weekday through
#' out 2023, starting with 2 Jan 2023. To ensure an adequate number of
#' transactions are observed for the estimation of the yield curve, the earliest
#' bond is issued prior to the beginning of 2023, determined by the length of
#' maturity of that type of bond, such that the last payment can still be
#' observed at the beginning of 2023. For example, the first bond with 20 years
#' of maturity is issued at the beginning of 2003. The last bond within this type
#' is issued at the end of 2023. The rest of the bonds have issue dates evenly
#' distributed between the first and the last bonds.
#'
#' The initial yield at the beginning of 2023 is generated from the following
#' equation
#' \deqn{Yield_{i, 0} = b_0 + b_1 * ((1 - \exp(-\tau_i / t_1)) / (\tau / t_1)) +
#' b_2 * ((1 - \exp(-\tau_i / t_2)) / (\tau_i / t_2) - \exp(-\tau_i / t_2))}
#' where \eqn{\tau_i} is the time to maturity in years. The yield curve at
#' quotation time \eqn{t} is obtained by multiplying this curve by the cubic
#' equation, \deqn{Yield_{i, t} = Yield_{i, 0} * (1 + linear * time + quadratic *
#' time^2 + cubic * time^3)} so the yield curve slowly changes over different
#' quotation dates. The time \eqn{t} is a value between 0 and 1, the proportion
#' of time that has passed by a quotation date, identifying the progression
#' through 2023. For example, the time \eqn{t} corresponding to 31 Mar 2023 is
#' 0.25.
#'
#' The discount function is then derived from the yield curve, \deqn{d_t(\tau) =
#' \exp(-\tau y_t(\tau))} to discount all the future cash flows of a bond and
#' calculate the price of that bond at the quotation date.
#'
#' @md
#' @inheritParams get_yield_at
#' @param n Integer. Number of bonds of each maturity to simulation
#'
#' @returns A [tibble::tibble()] object with 5 variables
#'
#' \describe{
#'   \item{qdate}{The quotation date of a bond in a [Date()] class.}
#'   \item{id}{The unique identifier of a bond.}
#'   \item{price}{The price of a bond.}
#'   \item{tupq}{The remaining time until the given cash flow in days.}
#'   \item{pdint}{The payment amount of the cash flow.
#'     For a non-coupon-paying bond, the only cash flow occurs on the maturity
#'     date with a payment of 100, i.e., the face value of the bond.
#'     For a coupon-paying bond, the stream of cash flows includes the coupon
#'     payable on the interest payment date before maturity and the face value
#'     100 plus the coupon payment for the last cash flow on the maturity date.}
#' }
#'
#'
#' @seealso [get_yield_at()]
#' @inherit get_yield_at references
#'
#' @examples
#' ycevo_data()
#'
#' @importFrom lubridate wday day days month day<- years
#' @importFrom rlang .env
#' @export
ycevo_data <- function(
    n = 40,
    b0 = 0, b1 = 0.05, b2 = 2,
    t1 = 0.75, t2 = 125,
    linear = -0.55, quadratic = 0.55, cubic = -0.55) {
  first_qdate <- ymd("20230101")
  last_qdate <- ymd("20231231")

  ad <- seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "1 day")
  wd <- ad[wday(ad, week_start = 1) < 6]

  sim_bond_meta <- function(n, arg_range_issued, random = TRUE){
    # # type = 1
    # # range of issue date in years before the first qdate
    # arg_range_issued <- c(20, 10)
    # # possible maturity in years
    # arg_maturity <- c(30)
    # # number of bonds
    # n <- 30
    if(random)
      issuedate <- first_qdate - days(round(runif(n, arg_range_issued[[2]], arg_range_issued[[1]])*365))
    else
      issuedate <- seq(first_qdate - days(round(arg_range_issued[[1]]*365)),
                       first_qdate - days(round(arg_range_issued[[2]]*365)),
                       length.out = n)
    # don't use leap day
    day(issuedate)[month(issuedate) == 2 & day(issuedate) == 29] <- 28
    tibble(issuedate)
  }

  is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  get_pqdate <- function(type, issuedate, matdate, ...){
    if(type == 4) return(matdate)
    # Payment every 6 months
    out <- seq(issuedate, matdate, by = "6 months")
    if(matdate - out[[length(out)]] > months(2)) {
      out <- c(out, matdate)
    } else {
    # If the last payment date is close to maturity date
    # Set them to be the same
      out[[length(out)]] <- matdate
    }
    out
  }
  bond_meta <- tibble::tribble(~ type, ~ n, ~ arg_range_issued, ~ maturity,
                               # Bonds 20 years maturity
                               1, n, c(20, -1), 20,
                               # Notes 2, 3, 5, 10 years maturity
                               2, n, c(2, -1), 2,
                               2, n, c(3, -1), 3,
                               2, n, c(5, -1), 5,
                               2, n, c(10, -1), 10,
                               # Bills 0.8 years maturity
                               4, n, c(0.8, -1), 0.8) %>%
    rowwise() %>%
    mutate(sim = list(sim_bond_meta(.data$n, .data$arg_range_issued, random = FALSE))) %>%
    ungroup() %>%
    select("type", "sim", "maturity") %>%
    unnest("sim") %>% # issuedate and maturity
    rowwise() %>%
    mutate(matdate = .data$issuedate + if(is_wholenumber(.data$maturity)) years(.data$maturity) else days(round(.data$maturity * 365))) %>%
    # couprt
    # based on bond type
    mutate(couprt = case_when(
      as.numeric(as.character(.data$type)) == 1 ~ repa(1, 7.5, 7^0.5),
      as.numeric(as.character(.data$type)) == 2 ~ repa(1, 4, 7^0.5),
      .default = 0)) %>%
    # id
    mutate(id = paste0(format(.data$matdate, "%Y%m%d"),
                       ".",
                       .data$type,
                       sprintf("%04d", round(100*.data$couprt)),
                       # make id unique
                       sample(0:9, 1)) %>%
             as.factor()) %>%
    mutate(pqdate = list(get_pqdate(.data$type, .data$issuedate, .data$matdate, .data$maturity))) %>%
    unnest("pqdate") %>%
    filter(.data$pqdate > .env$first_qdate)

  # find bonds with payment date after a date
  # and issue date before a date
  ls_row <- lapply(wd, function(wd) which(bond_meta$pqdate > wd & bond_meta$issuedate <= wd))
  n_wd <- vapply(ls_row, length, integer(1))
  cf <- bond_meta[unlist(ls_row),] %>%
    mutate(qdate = rep(.env$wd, .env$n_wd)) %>%
    arrange(.data$qdate, .data$id) %>%
    # Time to payment in days
    mutate(tupq = as.double(.data$pqdate - .data$qdate, units = "days")) %>%
    # Time to payment (maturity) in years
    mutate(ttm = .data$tupq/365) %>%
    # Payment
    # annual coupon /2 + face value if on maturity date
    # otherwise, annual coupon /2
    mutate(pdint = case_when(.data$pqdate == .data$matdate ~ .data$couprt/2 + 100,
                             .default = .data$couprt/2))

  # yield
  bond_yield <- cf %>%
    mutate(qdate_num = as.numeric(.data$qdate)) %>%
    # Time as t/T
    # value between 0 and 1
    mutate(xgrid = (.data$qdate_num - min(.data$qdate_num))/(max(.data$qdate_num) - min(.data$qdate_num))) %>%
    # Generate yield from Nelson and Siegel model
    mutate(yield = get_yield_at(.data$xgrid, .data$ttm,
                                b0 = b0, b1 = b1, b2 = b2,
                                t1 = t1, t2 = t2,
                                linear = linear, quadratic = quadratic, cubic = cubic))
  bond <- bond_yield %>%
    # Convert yield to discount function
    mutate(discount = exp(-.data$ttm * .data$yield)) %>%
    group_by(.data$qdate, .data$id) %>%
    # Discount future cashflows to current price
    mutate(price = sum(.data$pdint * .data$discount)) %>%
    ungroup() #%>%

  select(bond, "qdate", "id", "price", "tupq", "pdint")
}
