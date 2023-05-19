// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::export]]
arma::mat calc_dbar_c(int ntupq, arma::mat day_idx, 
                      arma::mat tupq_idx, arma::mat mat_weights_tau, 
                      arma::mat mat_weights_qdatetime,
                      Rcpp::List price_slist, Rcpp::List cf_slist){
  arma::mat dbar(ntupq, 2);
  
  arma::rowvec seq_day = day_idx.row(0);
  
  // For each element in time-to-maturity tau grid
  for(int j = 0; j < ntupq; ++j){
    Rcpp::checkUserInterrupt();
    arma::rowvec seq_tupq(2);
    seq_tupq = tupq_idx.row(j);
    arma::vec num(seq_day[1] - seq_day[0] + 1, arma::fill::zeros);
    arma::vec den(seq_day[1] - seq_day[0] + 1, arma::fill::zeros);
    
    // For each element in the window of the current element of time grid: t=1 to T in the paper
    // The kernel for the rest is 0
    for(int k = seq_day[0] - 1; k < seq_day[1]; ++k){
      arma::sp_mat price_temp =  Rcpp::as<arma::sp_mat>(price_slist[k]);
      arma::sp_mat cf_temp =  Rcpp::as<arma::sp_mat>(cf_slist[k]);
      arma::sp_mat x = cf_temp.cols(seq_tupq[0] - 1, seq_tupq[1]-1);
      arma::uword q = seq_tupq[0] - 1;
      for(arma::sp_mat::const_iterator c = x.begin();
          c != x.end(); ++c) {
        num(k - seq_day[0] + 1) += price_temp(c.row(), c.col() + q) * *c * mat_weights_tau(c.col() + q, j) * mat_weights_qdatetime(k, 0);
        den(k - seq_day[0] + 1) += pow(*c, 2) * mat_weights_tau(c.col() + q, j) * mat_weights_qdatetime(k, 0);
      }
    }
    
    double numer = arma::sum(num), denom = arma::sum(den);
    dbar(j, 0) = numer;
    dbar(j, 1) = denom;
  }
  return dbar;
}

// [[Rcpp::export]]
arma::mat calc_hhat_num_c(int ntupq_tau, int ntupq_tau_p, arma::mat day_idx, 
                          arma::mat tupq_idx_tau, arma::mat tupq_idx_tau_p, 
                          arma::mat mat_weights_tau, arma::mat mat_weights_tau_p, 
                          arma::mat mat_weights_qdatetime, Rcpp::List cf_slist, 
                          bool same_tau) {
  
  arma::mat hhat(ntupq_tau, ntupq_tau_p);
  
  // For each element in xgrid time
  arma::rowvec seq_day = day_idx.row(0);
  
  int pp_start = 0;
  
  // For each element in tau grid
  for (int jj = 0; jj < ntupq_tau; ++jj) {
    arma::rowvec seq_tupq_x(2);
    seq_tupq_x = tupq_idx_tau.row(jj);
    
    // For each element in tau_p
    // The numerator of H_hat is symmetric 
    // so we only need to calculate half of it 
    // to cut time
    if(same_tau) pp_start = jj;
    for (int pp = pp_start; pp < ntupq_tau_p; ++pp) {
      Rcpp::checkUserInterrupt();
      arma::rowvec seq_tupq_q(2);
      seq_tupq_q = tupq_idx_tau_p.row(pp);
      
      for(int k = seq_day[0] - 1; k < seq_day[1]; ++k){
        arma::sp_mat cf_temp = Rcpp::as<arma::sp_mat>(cf_slist[k]);
        
        // Calculate the sum of p
        // excluding i later
        arma::sp_mat cf_temp_p(size(cf_temp));
        for(arma::sp_mat::const_iterator p = cf_temp.begin(); p != cf_temp.end(); ++p){
          cf_temp_p(p.row(), p.col()) =  *p * mat_weights_tau_p(p.col(), pp);
        }
        arma::sp_mat row_sum = sum(cf_temp_p.t(), 0);
        
        arma::sp_mat x = cf_temp.cols(seq_tupq_x[0] - 1, seq_tupq_x[1] - 1);
        arma::uword q = seq_tupq_x[0] - 1;
        for(arma::sp_mat::const_iterator j = x.begin(); j != x.end(); ++j) {
          hhat(jj, pp) += (row_sum(0, j.row()) - cf_temp_p(j.row(), j.col() + q)) * *j * 
            mat_weights_tau(j.col() + q, jj) * mat_weights_qdatetime(k, 0);
        }
      }
    }
  }
  return hhat;
}
