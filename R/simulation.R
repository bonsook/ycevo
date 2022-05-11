# @param frequency Integer scalar. The time interval between each qdate in days. Default to 2 weeks.
# @param time_span Numeric scalar. Time span in years. Default to 10 years
#' @importFrom lubridate ymd
#' @importFrom lubridate years
#' @importFrom lubridate days
sim_qdate <- function(frequency = 14, time_span = 10) {
  # seq(beginning <- ymd("2020-01-01"), beginning + years(time_span),  by = as.difftime(days(frequency)))
  c(
    # ymd("2019-01-01"),
    seq(beginning <- ymd("2020-01-01"), beginning + years(time_span),  
        by = lubridate::as.difftime(lubridate::days(frequency)))
  )
}


#' @param tau Bond maturity in years to choose from.
#' @importFrom lifecycle deprecated
#' @export
simulate_data_new <- function(
    time_span = 10,
    qdate_frequency = 14,
    # maturity = 36, 
    tau = c(seq(30, 6 * 30, 30),  
            seq(240, 2 * 365, 60)) / 365,
    nbonds = 24, 
    coupon_rates = function(){1:5}, 
    sdev = 0.1, 
    arma_terms = list(ar = 0.1, ma = 0),
    yield = NULL,
    max_qDate = deprecated(), 
    periods = deprecated(), 
    bond_multiplier = deprecated(),
    coupon_frequency = deprecated() 
) {
  if(lifecycle::is_present(periods)) {
    lifecycle::deprecate_warn("1.1.0", "simulation(periods)", "simulate_data(tau)")
  }
  if(lifecycle::is_present(bond_multiplier)) {
    lifecycle::deprecate_warn("1.1.0", "simulation(bond_multiplier)", "simulate_data(nbonds)")
  }
  if(lifecycle::is_present(coupon_frequency)) {
    lifecycle::deprecate_warn("1.1.0", "simulation(coupon_frequency)", 
                              details = "Coupons are paid biannually.")
  }
  if(lifecycle::is_present(max_qDate)) {
    lifecycle::deprecate_warn("1.1.0", "simulation(max_qDate)", "simulate_data(qdate_frequency)")
    return(simulate_data_old( 
      max_qDate = max_qDate, 
      periods = maturity, 
      bond_multiplier = bond_multiplier, 
      coupon_frequency = coupon_frequency, 
      coupon_rates = coupon_rates, 
      sdev = sdev, 
      arma_terms = arma_terms,
      yield = yield))
    
    
  }
  
  # tauSeq <- c(seq(30, 6 * 30, 30)
  #             ,  # Monthly up to six months
  #             seq(240, 2 * 365, 60)
  #             # ,  # Two months up to two years
  #             # seq(2 * 365 + 90, 5 * 365, 90),  # Three months up to five years
  #             # seq(5 * 365+ 120, 10 * 365, 120) # four months up to ten years
  #             # , seq(10 * 365 + 365, 35 * 365, 365) # Annually up to 35 years
  # ) / 365
  tauSeq <- tau
  
  # if(length(maturity) == 1) {
  #   tauSeq <- (1:maturity)/(maturity/time_span)
  # } else {
  #   tauSeq <- maturity
  # }
  qDates <- sim_qdate(frequency = qdate_frequency, time_span = time_span)
  n_qdates <- length(qDates)
  n_maturity <- length(tauSeq)
  
  # yield <- sapply(
  #   seq_len(n_qdates)/n_qdates, 
  #   function(time) sapply(
  #     tauSeq, 
  #     function(maturity) get_yield_at(
  #       time, maturity)))
  # discount <- exp(-tauSeq * yield)
  
  base_bond <- tibble(mat_days = as.integer(tauSeq*365)) %>% 
    mutate(base_id = row_number())
  
  {
    bond_env <- new.env()
    bond_env$index <- 1
    give_index <- function(n) {
      output <- seq(bond_env$index, by = 1, length.out = n)
      bond_env$index <- output[[n]] + 1
      output
    }
    bond_env$coupon <- data.frame(crspid = numeric(), coupon = numeric())
    give_coupon_one <- function(crspid, mat_days){
      if(all(bond_env$coupon$crspid != crspid)) {
        bond_env$coupon <- bind_rows(
          bond_env$coupon, 
          tibble(crspid = crspid) %>% 
            mutate(
              coupon = ifelse(
                mat_days <= 365, 
                ifelse(
                  mat_days < 365/2, 
                  0, 
                  ifelse(
                    runif(1) < 0.5,
                    0,
                    sample(coupon_rates(), 1)
                  )
                ), 
                sample(coupon_rates(), 1)
              )
            )
        )
      }
      bond_env$coupon$coupon[bond_env$coupon$crspid == crspid]
    }
    give_coupon <- function(crspid, mat_days) {
      mapply(give_coupon_one, 
             crspid, mat_days)
    }
    
    }
  
  bond_list <- vector("list", n_qdates)
  bond_list[[1]] <- slice_sample(base_bond, n=nbonds, replace = TRUE) %>% 
    mutate(issue_date  = sample(seq(ymd("2019-01-01"), qDates[[1]], lubridate::as.difftime(days(1))), nrow(.), replace = TRUE)) %>% 
    mutate(mat_date = issue_date + days(mat_days)) %>% 
    mutate(qdate = issue_date) %>% 
    mutate(crspid = give_index(n()))
  
  for( i in seq_along(qDates)[-1]) {
    current_bonds <- old_bonds <- filter(bond_list[[i-1]], mat_date > qDates[[i]])
    expired_bonds <- filter(bond_list[[i-1]], mat_date <= qDates[[i]])
    while(nrow(expired_bonds) != 0) {
      new_bonds <- base_bond %>%
        # filter(mat_days > qdate_frequency) %>% 
        slice_sample(n = nrow(expired_bonds), replace = TRUE) %>%  
        mutate(issue_date = expired_bonds$mat_date) %>% 
        mutate(mat_date = issue_date + days(mat_days)) %>% 
        mutate(crspid = give_index(n()))
      expired_bonds <- filter(new_bonds, mat_date <= qDates[[i]])
      current_bonds <- bind_rows(
        current_bonds, 
        anti_join(
          new_bonds, expired_bonds, 
          by = c("mat_days", "base_id", "issue_date", "mat_date", "crspid")))
    }  
    bond_list[[i]] <- current_bonds %>% 
      mutate(qdate = qDates[[i]])
  }
  
  
  bonds <- bind_rows(bond_list)
  
  output <- bonds %>% 
    rowwise() %>% 
    mutate(coupon = give_coupon(crspid, mat_days)) %>% 
    mutate(
      paydates = 
        ifelse(
          coupon==0, list(mat_date), 
          list(seq(mat_date, qdate + days(1), by = -lubridate::as.difftime(lubridate::months(6))))
        )
    ) %>% 
    tidyr::unnest_longer(paydates) %>% 
    mutate(pdint = case_when(
      mat_date == paydates ~ coupon + 100, 
      TRUE ~ coupon
    )) %>% 
    mutate(tumat = round(as.numeric(mat_date - qdate))) %>% 
    mutate(tupq = round(as.numeric(paydates - qdate))) %>% 
    group_by(crspid, qdate) %>% 
    mutate(accint = ifelse(
      coupon == 0, 0, 
      (183-min(tupq))/183*coupon
    )) 
  
  output <- output %>%
    filter(qdate >= ymd("2020-01-01")) %>% 
    # mutate(xgrid = (.data$qdate - min(.data$qdate))/max(.data$qdate)) %>% 
    # mutate(discount = exp(-tupq/365 * get_yield_at_vec(xgrid, tupq/365)))
    mutate(discount = exp(-tupq/365 * get_yield_at_vec(which(qDates == .data$qdate[[1]])/n_qdates, tupq/365)))
  
  error_df <- output %>% 
    distinct(crspid, qdate) %>% 
    group_by(crspid) %>% 
    mutate(error = stats::arima.sim(arma_terms, n()))
  output <- output %>% 
    left_join(error_df, by = c("qdate", "crspid")) %>% 
    # mutate(mid.price = sum(pdint * discount) + mean(sdev * sqrt(sqrt(tumat)) * error) + accint) 
    mutate(mid.price = sum(pdint * discount) + mean(sdev * sqrt(tupq) * error) + accint) 
  
  output %>%
    filter(qdate >= ymd("2020-01-01")) %>% 
    # select(qdate, crspid, mid.price, tupq, pdint, tumat, accint) %>% 
    ungroup()
  
  
}
