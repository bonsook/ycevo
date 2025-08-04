#' Nonparametric Estimation of the Yield Curve Evolution
#'
#' Nonparametric estimation of discount functions and yield curves.
#' @aliases ycevo-package
#' @docType package
#' @keywords package
#'
#' @importClassesFrom Matrix dgCMatrix
## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr ends_with
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom lubridate ymd
#' @importFrom Matrix colSums rowSums sparseMatrix t
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang !! sym .data
#' @importFrom rlang %||%
#' @importFrom rlang :=
#' @importFrom rlang syms
#' @importFrom stats runif
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
## usethis namespace: end
#' @useDynLib ycevo
#' @references Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a nonparametric model for bond prices from cross-section and time series information. Journal of Econometrics, 220(2), 562-588.
"_PACKAGE"


utils::globalVariables(c("."))
