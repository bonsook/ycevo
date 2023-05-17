calc_dbar_r <- function(nday, ntupq, day_idx, tupq_idx, mat_weights_tau, mat_weights_qdatetime, price_slist, cf_slist){
  dbar <- matrix(nrow = nday * ntupq, ncol = 2)
  
  seq_day <- day_idx[1,]
  
  # j <- 1
  for(j in seq_len(nrow(tupq_idx))){
    
    seq_tupq <- tupq_idx[j,]
    
    num <- numeric(seq_day[[2]] - seq_day[[1]] +1)
    den <- numeric(seq_day[[2]] - seq_day[[1]] +1)
    
    # k <- 1
    for(k in seq(seq_day[[1]], seq_day[[2]])){
      
      price_temp <- price_slist[[k]]  
      cf_temp <- cf_slist[[k]]
      ncols <- ncol(price_temp)
      
      # m <- 1
      for(m in seq_len(nrow(price_temp))){
        # n <- 1
        for(n in seq(seq_tupq[[1]], min(seq_tupq[[2]], ncols))){
          num[[k-seq_day[[1]] + 1]] <- num[[k-seq_day[[1]] + 1]] + price_temp[m, n] * cf_temp[m,n] * mat_weights_tau[n, j] * mat_weights_qdatetime[k,1]
          den[[k-seq_day[[1]] + 1]] <- den[[k-seq_day[[1]] + 1]] + cf_temp[m,n]^2 * mat_weights_tau[n, j] * mat_weights_qdatetime[k,1]
        }
      }
      
    }
    dbar[[j, 1]] <- sum(num)
    dbar[[j, 2]] <- sum(den)
    
  }
  dbar
  
}

calc_dbar_r2 <- function(nday, ntupq, day_idx, tupq_idx, mat_weights_tau, mat_weights_qdatetime, price_slist, cf_slist){
  
  seq_day <- day_idx[1,]
  vapply(seq_len(nrow(tupq_idx)), function(j){
    seq_tupq <- tupq_idx[j,]
    
    vapply(seq(seq_day[[1]], seq_day[[2]]), function(k){
      price_temp <- price_slist[[k]]  
      cf_temp <- cf_slist[[k]]
      ncols <- ncol(price_temp)
      seq_t <- seq(seq_tupq[[1]], min(seq_tupq[[2]], ncols))
      num <- (price_temp[, seq_t] * cf_temp[, seq_t]) %*% (mat_weights_tau[seq_t, j] * mat_weights_qdatetime[k,1])
      den <- (cf_temp[, seq_t]^2) %*% (mat_weights_tau[seq_t, j] * mat_weights_qdatetime[k,1])
      c(sum(num), sum(den))
    }, FUN.VALUE = numeric(2)) %>% 
      rowSums()
    
  }, FUN.VALUE = numeric(2)) %>% 
    t()
  
  
}
calc_dbar_r3 <- function(nday, ntupq, day_idx, tupq_idx, mat_weights_tau, mat_weights_qdatetime, price_slist, cf_slist){
  
  seq_day <- day_idx[1,]
  # stopifnot(length(unique(vapply(price_slist, ncol, integer(1)))) == 1)
  # seq_t_ls <- apply(tupq_idx, 1, function(x) seq(x[[1]], x[[2]])) 
  
  vapply(seq(seq_day[[1]], seq_day[[2]]), function(k){
    price_temp <- price_slist[[k]]  
    cf_temp <- cf_slist[[k]]
    num <- (price_temp * cf_temp) %*% mat_weights_tau * mat_weights_qdatetime[k,1]
    den <- (cf_temp^2) %*% mat_weights_tau * mat_weights_qdatetime[k,1]
    rbind(colSums(num), colSums(den))
  }, FUN.VALUE = matrix(NA_real_, ncol = nrow(tupq_idx), nrow = 2)) %>% 
    rowSums(dims = 2L) %>% 
    t()
}

calc_dbar_r4 <- function(mat_weights_tau, mat_weights_qdatetime, price_smat, cf_smat){
  
  ns <- nrow(price_smat)/nrow(mat_weights_qdatetime)
  # tempi <- matrix(0, nrow = nrow(mat_weights_qdatetime), ncol = nrow(price_smat))
  tempi <- sparseMatrix(i = rep(seq_len(nrow(mat_weights_qdatetime)), each = ns),
                        j = seq_len(nrow(price_smat)),
                        x = 1,
                        dims = c(nrow(mat_weights_qdatetime), nrow(price_smat)))
  
  num <- Matrix::crossprod(mat_weights_qdatetime, tempi) %*% (price_smat * cf_smat) %*% mat_weights_tau
  den <- Matrix::crossprod(mat_weights_qdatetime, tempi) %*% (cf_smat^2) %*% mat_weights_tau
  t(rbind(num, den))
  
}


