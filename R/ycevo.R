#' Import functions
#' @name imports
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Rcpp evalCpp
#' @importFrom Matrix colSums rowSums sparseMatrix t
#' @importFrom dplyr filter left_join mutate select group_by lead lag
#' @importFrom rlang !! sym
#' @importFrom stats var
#' @importFrom magrittr %>%
NULL

#' Package 'ycevo'
#' @name ycevo
#' @title Estimation of Yield Curve Dynamics
#' @author Bonsoo Koo <bonsoo.koo@monash.edu>,
#' Kai-Yang Goh <kai-yang.goh@monash.edu>
#' Nathaniel Tomasetti <nathaniel.tomasetti@gmail.com>
#' @description This package estimates the dynamics of the yield curve using nonparametric
#' methods and tests for stationarity.
NULL

#' @useDynLib ycevo
#' @importFrom Rcpp sourceCpp
NULL
