#' @importFrom lubridate wday day month day<- years
#' @importFrom rlang .env
#' @export
ycevo_data <- function(b0 = 0, b1 = 0.05, b2 = 2, 
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
    out <- seq(issuedate, matdate, by = "6 months")
    if(matdate - out[[length(out)]] > months(2)) {
      out <- c(out, matdate)
    } else {
      out[[length(out)]] <- matdate
    }
    out
  }
  bond_meta <- tibble::tribble(~ type, ~ n, ~ arg_range_issued, ~ maturity,
                       1, 40, c(20, -1), 20,
                       2, 40, c(2, -1), 2,
                       2, 40, c(3, -1), 3,
                       2, 40, c(5, -1), 5,
                       2, 40, c(10, -1), 10,
                       4, 40, c(0.8, -1), 0.8) %>%
    rowwise() %>%
    mutate(sim = list(sim_bond_meta(.data$n, .data$arg_range_issued, random = FALSE))) %>%
    ungroup() %>%
    select("type", "sim", "maturity") %>%
    unnest("sim") %>% # issuedate and maturity
    rowwise() %>%
    mutate(matdate = .data$issuedate + if(is_wholenumber(.data$maturity)) years(.data$maturity) else days(round(.data$maturity * 365))) %>%
    # couprt
    mutate(couprt = case_when(
      as.numeric(as.character(.data$type)) == 1 ~ repa(1, 7.5, 7^0.5),
      as.numeric(as.character(.data$type)) == 2 ~ repa(1, 4, 7^0.5),
      .default = 0)) %>%
    # id
    mutate(id = paste0(format(.data$matdate, "%Y%m%d"),
                       ".",
                       .data$type,
                       sprintf("%04d", round(100*.data$couprt)),
                       sample(0:9, 1)) %>%
             as.factor()) %>%
    mutate(pqdate = list(get_pqdate(.data$type, .data$issuedate, .data$matdate, .data$maturity))) %>%
    unnest("pqdate") %>%
    filter(.data$pqdate > .env$first_qdate)
  
  ls_row <- lapply(wd, function(wd) which(bond_meta$pqdate > wd & bond_meta$issuedate <= wd))
  n_wd <- vapply(ls_row, length, integer(1))
  cf <- bond_meta[unlist(ls_row),] %>%
    mutate(qdate = rep(.env$wd, .env$n_wd)) %>%
    arrange(.data$qdate, .data$id) %>%
    mutate(tupq = as.double(.data$pqdate - .data$qdate, units = "days")) %>%
    mutate(ttm = .data$tupq/365) %>%
    mutate(pdint = case_when(.data$pqdate == .data$matdate ~ .data$couprt/2 + 100,
                             .default = .data$couprt/2))
  
  # yield
  bond_yield <- cf %>%
    mutate(qdate_num = as.numeric(.data$qdate)) %>%
    mutate(xgrid = (.data$qdate_num - min(.data$qdate_num))/(max(.data$qdate_num) - min(.data$qdate_num))) %>%
    mutate(yield = get_yield_at(.data$xgrid, .data$ttm,
                                b0 = b0, b1 = b1, b2 = b2, 
                                t1 = t1, t2 = t2,
                                linear = linear, quadratic = quadratic, cubic = cubic))
  bond <- bond_yield %>%
    mutate(discount = exp(-.data$ttm * .data$yield)) %>%
    group_by(.data$qdate, .data$id) %>%
    mutate(mid.price = sum(.data$pdint * .data$discount)) %>%
    ungroup() #%>%
  
  select(bond, "qdate", "id", "mid.price", "tupq", "pdint")
}
