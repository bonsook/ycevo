// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::export]]
arma::mat calc_dbar_c(int nday, int ntupq, arma::mat day_idx, arma::mat tupq_idx, arma::mat ux_window, arma::mat uu_window,
                      Rcpp::List price_slist, Rcpp::List cf_slist){
  arma::mat dbar(nday * ntupq, 2);
  
  // For each element in ugrid
  for(int i = 0; i < nday; ++i){
    arma::rowvec seq_day = day_idx.row(i);
    
    // For each element in xgrid
    for(int j = 0; j < ntupq; ++j){
      Rcpp::checkUserInterrupt();
      arma::rowvec seq_tupq;
      int windowOffset = 0;
      if(tupq_idx.n_cols == 2){
        seq_tupq = tupq_idx.row(j);
      } else {
        seq_tupq = tupq_idx(j, arma::span(2*i, 2*i+1));
        windowOffset = i * ntupq;
      }
      arma::vec num(seq_day[1] - seq_day[0] + 1, arma::fill::zeros), den(seq_day[1] - seq_day[0] + 1, arma::fill::zeros);
      
      // For each element in the window of the current element of ugrid: t=1 to T in the paper
      // The kernel for the rest is 0
      for(int k = seq_day[0] - 1; k < seq_day[1]; ++k){
        arma::sp_mat price_temp =  Rcpp::as<arma::sp_mat>(price_slist[k]);
        arma::sp_mat cf_temp =  Rcpp::as<arma::sp_mat>(cf_slist[k]);
        double ncols = price_temp.n_cols;
        
        // The next two loops are to loop over elements in the cash flow and price matrix in the price_slist and cf_slist
        // For each bond: i=1 to n_t in the paper
        for(unsigned int m = 0; m < price_temp.n_rows; ++m){
          
          // For each element in the window of the current element of xgrid: j=1 to m_it in the paper
          // The kernel for the rest is 0
          for(int n = seq_tupq[0] - 1; n < std::min(seq_tupq[1], ncols); ++n) {
            num(k - seq_day[0] + 1) += price_temp(m, n) * cf_temp(m, n) * ux_window(n, windowOffset + j) * uu_window(k, i);
            den(k - seq_day[0] + 1) += pow(cf_temp(m, n), 2) * ux_window(n, windowOffset + j) * uu_window(k, i);
          }
        }
      }
      
      double numer = arma::sum(num), denom = arma::sum(den);
      dbar(i*ntupq + j, 0) = numer;
      dbar(i*ntupq + j, 1) = denom;
    }
  }
  return dbar;
}

arma::mat check_crspid_intersect (Rcpp::List cf_slist, arma::rowvec seq_tupq_x, arma::rowvec seq_tupq_q, arma::rowvec seq_day){
  arma::sp_mat tempMat = Rcpp::as<arma::sp_mat>(cf_slist[seq_day[0]-1]);
  int N = tempMat.n_rows, M = cf_slist.length();
  // Row number is the total number of crspid in the data
  // Column number is the number of qdate
  arma::mat intMatrix (N, M);
  for(int i = seq_day[0] - 1; i < seq_day[1]; ++i){
    tempMat =  Rcpp::as<arma::sp_mat>(cf_slist[i]);
    arma::vec intVec (N, arma::fill::zeros);
    int counter = 0;
    double ncols = tempMat.n_cols;
    for(int k = 0; k < N; ++k){
      double xRowSum = arma::sum(arma::sum(tempMat(k, arma::span(seq_tupq_x[0] - 1, std::min(ncols - 1, seq_tupq_x[1] - 1)))));
      double qRowSum = arma::sum(arma::sum(tempMat(k, arma::span(seq_tupq_q[0] - 1, std::min(ncols - 1, seq_tupq_q[1] - 1)))));
      if((xRowSum > 0) & (qRowSum > 0)){
        intVec(counter) = k;
        counter += 1;
      }
    }
    intMatrix.col(i) = intVec;
  }
  return intMatrix;
}

// Reweight weights for tau_p so the weights at where there is a cash flow add up to one
arma::cube weightCF2(arma::vec qdate_idx, Rcpp::List cf_slist, arma::rowvec seq_tupq, arma::mat crspid_idx, arma::mat window, int i){
  unsigned int maxRows = 0;
  for(int j = qdate_idx(0); j <= qdate_idx(qdate_idx.n_elem - 1); ++j){
    for(unsigned int k = 0; k < crspid_idx.n_rows; ++k){
      if(crspid_idx(k, j) == 0){
        if(k > maxRows){
          maxRows = k;
        }
        break;
      }
    }
  }
  // Rprintf("maxRows %i\n", maxRows);
  arma::cube cfTemp(maxRows, seq_tupq[1] - seq_tupq[0] + 1, qdate_idx.n_elem, arma::fill::zeros);
  
  for(unsigned int j = 0; j < qdate_idx.n_elem; ++j){
    arma::sp_mat tempMat = Rcpp::as<arma::sp_mat>(cf_slist[qdate_idx(j)]);
    arma::vec tempVec = crspid_idx.col(qdate_idx(j));
    
    arma::mat Weights (maxRows, seq_tupq[1] - seq_tupq[0] + 1, arma::fill::zeros);
    double ncols = tempMat.n_cols;
    for(unsigned int k = 0; k < maxRows; ++k){
      if(tempVec(k) != 0){
        
        // reweight
        double wsum = 0;
        int counter = 0;
        for(int l = seq_tupq[0] - 1; l < std::min(ncols, seq_tupq[1]); ++l){
          if(tempMat(tempVec(k), l) > 0){
            wsum += window(l, i);
            counter += 1;
          }
        }
        // if there are more than 5 cash flow
        // do not re weight
        if(counter >= 5) {
          wsum = 1;
        }
        
        
        for(int l = seq_tupq[0] - 1; l < std::min(ncols, seq_tupq[1]); ++l){
          Weights(k, l - seq_tupq[0] + 1) = tempMat(tempVec(k), l) * (window(l, i)/wsum);
        }
      } else {
        break;
      }
    }
    cfTemp.slice(j) = Weights;
  }
  return cfTemp;
}

arma::cube weightCF(arma::vec qdate_idx, Rcpp::List cf_slist, arma::rowvec seq_tupq, arma::mat crspid_idx, arma::mat window, int i){
  unsigned int maxRows = 0;
  for(int j = qdate_idx(0); j <= qdate_idx(qdate_idx.n_elem - 1); ++j){
    for(unsigned int k = 0; k < crspid_idx.n_rows; ++k){
      if(crspid_idx(k, j) == 0){
        if(k > maxRows){
          maxRows = k;
        }
        break;
      }
    }
  }
  arma::cube cfTemp(maxRows, seq_tupq[1] - seq_tupq[0] + 1, qdate_idx.n_elem, arma::fill::zeros);
  
  for(unsigned int j = 0; j < qdate_idx.n_elem; ++j){
    arma::sp_mat tempMat = Rcpp::as<arma::sp_mat>(cf_slist[qdate_idx(j)]);
    arma::vec tempVec = crspid_idx.col(qdate_idx(j));
    
    arma::mat Weights (maxRows, seq_tupq[1] - seq_tupq[0] + 1, arma::fill::zeros);
    double ncols = tempMat.n_cols;
    for(unsigned int k = 0; k < maxRows; ++k){
      if(tempVec(k) != 0){
        for(int l = seq_tupq[0] - 1; l < std::min(ncols, seq_tupq[1]); ++l){
          Weights(k, l - seq_tupq[0] + 1) = tempMat(tempVec(k), l) * window(l, i);
        }
      } else {
        break;
      }
    }
    cfTemp.slice(j) = Weights;
  }
  return cfTemp;
}

arma::cube createSumP (arma::cube cf_temp_q, arma::rowvec seq_tupq_x){
  arma::cube sumP(cf_temp_q.n_rows, seq_tupq_x[1] - seq_tupq_x[0] + 1, cf_temp_q.n_slices);
  for(unsigned int i= 0; i < cf_temp_q.n_slices; ++i){
    for(unsigned int k = 0; k < cf_temp_q.n_rows; ++k){
      sumP.slice(i).row(k).fill(arma::sum(cf_temp_q.slice(i).row(k)));
    }
  }
  return sumP;
}

double calc_hhat_once(int j, int k, arma::rowvec seq_day, arma::rowvec seq_tupq_x, arma::rowvec seq_tupq_q,
                      Rcpp::List cf_slist, arma::mat ux_window, arma::mat uq_window, arma::colvec uu_window){
  
  // Returns bonds that there is a cashflow in both the x window and q window -> for crossproducts?
  arma::mat crspid_idx = check_crspid_intersect(cf_slist, seq_tupq_x, seq_tupq_q, seq_day);
  //Matches the above to the day sequence for the u window
  arma::vec qdate_idx(seq_day[1] - seq_day[0] + 1);
  int counter = 0;
  for(int m = seq_day[0] - 1; m < seq_day[1]; ++m){
    if(arma::sum(crspid_idx.col(m)) != 0){
      qdate_idx[counter] = m;
      counter += 1;
    }
  }
  // Skip this iteration if there are no payments in q
  if(counter == 0) {
    return 0;
  }
  // Otherwise grab the relevant parts of qdate
  qdate_idx = qdate_idx.head(counter);
  // Weights x and q cash flows by the window function
  arma::cube cf_temp_x = weightCF(qdate_idx, cf_slist, seq_tupq_x, crspid_idx, ux_window, j);
  arma::cube cf_temp_q = weightCF2(qdate_idx, cf_slist, seq_tupq_q, crspid_idx, uq_window, k);
  // Creates cf_temp_q sized matrices where each element is the sum of that row of cf_temp_q
  arma::cube sumP = createSumP (cf_temp_q, seq_tupq_x);
  // Checks intersections of x and q.
  arma::vec j_idx(seq_tupq_x(1) - seq_tupq_x(0) + 1);
  arma::vec p_idx(seq_tupq_q(1) - seq_tupq_q(0) + 1);
  counter = 0;
  for(int m = 0; m <= seq_tupq_x(1) - seq_tupq_x(0); ++m){
    for(int n = 0; n <= seq_tupq_q(1) - seq_tupq_q(0); ++n){
      if(m + seq_tupq_x(0) == n + seq_tupq_q(0)){
        j_idx(counter) = m;
        p_idx(counter) = n;
        counter += 1;
      }
    }
  }
  if(counter == 1){
    for(unsigned int z = 0; z < qdate_idx.n_elem; ++z){
      sumP.slice(z).col(j_idx[0]) -= sumP.slice(z).col(p_idx[0]);
    }
  } else if(counter > 1){
    for(unsigned int z = 0; z < qdate_idx.n_elem; ++z){
      sumP.slice(z).cols(j_idx[0], j_idx[counter - 1]) -= cf_temp_q.slice(z).cols(p_idx[0], p_idx[counter - 1]);
    }
  }
  // Multiplies the adjusted sum_p matrix by the actual cashflows.
  double hhat_num = 0;
  for(unsigned int m = 0; m < sumP.n_slices; ++m){
    double sumCrossProduct = arma::accu(sumP.slice(m) % cf_temp_x.slice(m));
    hhat_num += sumCrossProduct * uu_window(qdate_idx(m));
  }
  return hhat_num;
}

// [[Rcpp::export]]
arma::cube calc_hhat_num2_c(int nday, int ntupq_x, int ntupq_q, arma::mat day_idx, arma::mat tupq_idx_x, arma::mat tupq_idx_q, arma::mat ux_window, arma::mat uq_window, arma::mat uu_window, Rcpp::List cf_slist) {
  
  arma::cube hhat(ntupq_x, ntupq_q, nday, arma::fill::zeros);
  
  // For each element in ugrid time
  for (int i = 0; i < nday; ++i) {
    arma::rowvec seq_day = day_idx.row(i);
    
    // For each element in xgrid tau
    for (int j = 0; j < ntupq_x; ++j) {
      arma::rowvec seq_tupq_x;
      int window_lower_x, window_upper_x;
      if(tupq_idx_x.n_cols == 2){
        seq_tupq_x = tupq_idx_x.row(j);
        window_lower_x = 0;
        window_upper_x = ntupq_x - 1;
      } else {
        seq_tupq_x = tupq_idx_x(j, arma::span(2*i, 2*i+1));
        window_lower_x = ntupq_x * i;
        window_upper_x = ntupq_x * (i + 1) - 1;
      }
      
      // For each element in qgrid tau_p
      for (int k = 0; k < ntupq_q; ++k) {
        Rcpp::checkUserInterrupt();
        arma::rowvec seq_tupq_q;
        int window_lower_q, window_upper_q;
        if(tupq_idx_q.n_cols == 2){
          seq_tupq_q = tupq_idx_q.row(k);
          window_lower_q = 0;
          window_upper_q = ntupq_q - 1;
        } else {
          seq_tupq_q = tupq_idx_q(k, arma::span(2*i, 2*i+1));
          window_lower_q = ntupq_q * i;
          window_upper_q = ntupq_q * (i + 1) - 1;
        }
        hhat(j, k, i) = calc_hhat_once(j, k, seq_day, seq_tupq_x, seq_tupq_q, cf_slist,
             ux_window.cols(window_lower_x, window_upper_x), uq_window.cols(window_lower_q, window_upper_q), uu_window.col(i));
      }
      
    }
  }
  return hhat;
}
