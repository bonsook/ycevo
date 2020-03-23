

#' Non-Parametric Estimation of the Yield Curve Evolution
#' 
#' Non-parametric estimation of the discount rate and yield curve.
#' 
#' @name ycevo
#' @docType package
#' @keywords package
#' 
#' @author Bonsoo Koo. \email{bonsoo.koo@@monash.edu}
#' @author Kai-Yang Goh. \email{kai-yang.goh@@monash.edu}
#' @author Nathaniel Tomasetti. \email{nathaniel.tomasetti@@gmail.com}
#' @author Yangzhuoran Yang. \email{fin.yang@@monash.edu}
#' 
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Rcpp evalCpp
#' @importFrom Matrix colSums rowSums sparseMatrix t
#' @importFrom dplyr filter left_join mutate select group_by lead lag
#' @importFrom rlang !! sym
#' @importFrom stats var
#' @importFrom magrittr %>%
#' @importFrom Rcpp sourceCpp
#' @useDynLib ycevo
NULL


utils::globalVariables(c("."))