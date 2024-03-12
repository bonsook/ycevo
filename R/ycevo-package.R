

#' Nonparametric Estimation of the Yield Curve Evolution
#' 
#' Nonparametric estimation of the discount rate and yield curve.
#' @aliases ycevo-package
#' @docType package
#' @keywords package
#' 
#' @author Bonsoo Koo \email{bonsoo.koo@@monash.edu}
#' @author Kai-Yang Goh \email{kai-yang.goh@@monash.edu}
#' @author Nathaniel Tomasetti \email{nathaniel.tomasetti@@gmail.com}
#' @author Yangzhuoran Fin Yang (Maintainer) \email{fin.yang@@monash.edu}
#' 
#' @importClassesFrom Matrix dgCMatrix
## usethis namespace: start
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr ends_with
#' @importFrom dplyr filter left_join mutate select group_by lead lag group_split ungroup
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr rowwise
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom Matrix colSums rowSums sparseMatrix t
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang :=
#' @importFrom rlang !! sym .data
#' @importFrom rlang %||%
#' @importFrom rlang syms
#' @importFrom stats runif
#' @importFrom stats var
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
## usethis namespace: end
#' @useDynLib ycevo
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a nonparametric model for bond prices from cross-section and time series information. Journal of Econometrics, 220(2), 562-588.
"_PACKAGE"



utils::globalVariables(c("."))
