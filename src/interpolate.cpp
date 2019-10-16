// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

//' One dimensional interpolation over ugrid
//'
//' Given the inputs, this interpolates dhat at a given xgrid over the nearest ugrid values
//' @param x, the value to be interpolated
//' @param u, the value to be interpolated
//' @param ugrid, vector, the range of u values we have data for
//' @param interp, matrix, element i,j gives the dhat value of the i'th xval and j'th ugrid
//' @param xvals, vector, the unique values of xvals that are included in interp. x must appear in this vector.
//' @export
// [[Rcpp::export]]
double interpolate_ugrid(double x, double u, arma::vec ugrid, arma::mat interp, arma::vec xvals){
  int length_u = ugrid.n_elem;
  int length_x = xvals.n_elem;
  int row = 0;
  for(int i = 0; i < length_x; ++i){
    if(xvals(i) == x){
      row = i;
      break;
    }
  }
  if(u <= ugrid(0)){
    return(interp(row, 0));
  } else if(u >= ugrid(length_u - 1)){
    return(interp(row, length_u - 1));
  } else {
    double uL, uU, xL, xU;
    for(int i = 0; i < length_u; ++i){
      if(ugrid(i) > u){
        uL = ugrid(i-1);
        uU = ugrid(i);
        xL = interp(row, i-1);
        xU = interp(row, i);
        return(xL + (u - uL) * (xU - xL) / (uU - uL));
      }
    }
  }
  return(-1);
}

//' Two dimensional interpolation method
//'
//' Given the inputs, this interpolates the dhat value at a given xgrid value over both ugrid and rgrid.
//' @param x, the value to be interpolated
//' @param u, the value to be interpolated
//' @param r, the value to be interpolated
//' @param ugrid, vector, the range of u values we have data for
//' @param rgrid, matrix, each row corresponds to the xgrid values for a given ugrid.
//' Pad end of a row with duplicates if necessary to make a matrix
//' @param xvals, vector, the unique values of x we have a dhat for, corresponds to interp rows
//' @param interp, three dimensional array, element i,j,k gives the dhat value for the i'th value of xvals, the j'th value of ugrid and k'th value of rgrid.
//' @export
// [[Rcpp::export]]
double interpolate_ugrid_rgrid(double x, double u, double r, arma::vec ugrid, arma::mat rgrid, arma::cube interp, arma::vec xvals){
  int length_u = ugrid.n_elem;
  int length_x = interp.n_rows;
  int length_r = rgrid.n_cols;
  int row = 0;
  double xInt, xLower, xUpper;
  int rVal, uVal, uLower, uUpper;

  // First we check to see which element of xvals (and row of interp) corresponds to the given value of X.
  // In the paper we interpolate X via loess, so we can calculate the interp value for arbitary Xs and do not need to interpolate it here
  for(int i = 0; i < length_x; ++i){
    if(xvals(i) == x){
      row = i;
      break;
    }
  }
  // Second, Check if u lies outside the boundary of ugrid.
  if((u <= ugrid(0)) | (u >= ugrid(length_u - 1))){
    // u lies outside the boundary, we do not need to interpolate u and will hold it fixed at the boundary value.
    if(u <= ugrid(0)){
      uVal = 0;;
    } else {
      uVal = length_u - 1;
    }
    // Check if r lies outside the boundary for a given u value
    if((r <= rgrid(uVal, 0)) | (r >= rgrid(uVal, length_r - 1))){
      if(r <= rgrid(uVal, 0)){
        rVal = 0;
      } else {
        rVal = length_r - 1;
      }
      // U and R are both outside the boundary, set xInt to the boundary value
  ///Rprintf("%i %i %i lll", row, uVal, rVal);///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      xInt = interp(row, uVal, rVal);
    } else {
      // U is outside the boundary, R is inside: Interpolate R for the boundary U
      double rL, rU, xL, xU;
      for(int i = 0; i < length_r; ++i){
        if(rgrid(uVal, i) > r){
          rL = rgrid(uVal, i-1);
          rU = rgrid(uVal, i);
          xL = interp(row, uVal, i-1);
          xU = interp(row, uVal, i);
          xInt = xL + (r - rL) * (xU - xL) / (rU - rL);
        }
      }
    }
  } else {
    // U is outside the boundary. Pick the lower and upper value of U and interpolate between these two
    // We will calculate xLower and xUpper, the values of X interpolated over R, for these two values of U
    for(int i = 0; i < length_u; ++i){
      if(ugrid(i) > u){
        uLower = i - 1;
        uUpper = i;
        break;
      }
    }
    // Check if R is outside the boundary of R | uLower. If it is inside, interpolate R values for U = uLower
    if((r <= rgrid(uLower, 0)) | (r >= rgrid(uLower, length_r - 1))){
      if(r <= rgrid(uLower, 0)){
        rVal = 0;
      } else {
        rVal = length_r - 1;
      }
      xLower = interp(row, uLower, rVal);
    } else {
      double rL, rU, xL, xU;
      for(int i = 0; i < length_r; ++i){
        if(rgrid(uLower, i) > r){
          rL = rgrid(uLower, i-1);
          rU = rgrid(uLower, i);
          xL = interp(row, uLower, i-1);
          xU = interp(row, uLower, i);
          xLower = xL + (r - rL) * (xU - xL) / (rU - rL);
        }
      }
    }
    // Check if R is outside the boundary of R | uUpper. If it is inside, interpolate R values for U = uUpper
    if((r <= rgrid(uUpper, 0)) | (r >= rgrid(uUpper, length_r - 1))){
      if(r <= rgrid(uUpper, 0)){
        rVal = 0;
      } else {
        rVal = length_r - 1;
      }
      xLower = interp(row, uUpper, rVal);
    } else {
      double rL, rU, xL, xU;
      for(int i = 0; i < length_r; ++i){
        if(rgrid(uUpper, i) > r){
          rL = rgrid(uUpper, i-1);
          rU = rgrid(uUpper, i);
          xL = interp(row, uUpper, i-1);
          xU = interp(row, uUpper, i);
          xUpper = xL + (r - rL) * (xU - xL) / (rU - rL);
        }
      }
    }
    // Interpolate over U for the interpolated xLower / xUpper
    double uL = ugrid(uLower);
    double uU = ugrid(uUpper);

    xInt = xLower + (u - uL) * (xUpper - xLower) / (uU - uL);
  }
  return(xInt);
}
