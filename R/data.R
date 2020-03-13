#' CRSP US Bond Dataset from 02/01/2007 to 31/12/2007
#'
#' A dataset containing the prices and other attributes
#' of CRSP US treasury bills, notes, and bonds.
#' Columns qdate, crspid, tumat, mid.price, accint, pdint and tupq are required for estimation.
#' The dataset includes all the dates from 2001 to 2007, only for the purpose of providing the qdates.
#' The variables for entries before 2007 are only repetitions other than qdate.
#' @format A data frame 
#' \describe{
#'   \item{qdate}{Quotation Date}
#'   \item{crspid}{Bond Identifier}
#'   \item{type}{1: Long Term Bonds, 2: Short Term Bonds, 4: Zero Coupon Bonds}
#'   \item{couprt}{Coupon Rate}
#'   \item{matdate}{Bond Maturity Date}
#'   \item{tumat}{Days to Maturity}
#'   \item{mid.price}{Quoted Mid-Price}
#'   \item{accint}{Accumulated Interest}
#'   \item{issuedate}{Bond Issue Date}
#'   \item{pqdate}{Bond Payment Date}
#'   \item{pdint}{Bond Payment Amount}
#'   \item{tupq}{Days to next Bond Payment}
#'   \item{year}{Year of the Bound Payment}
#' }
#' @source \url{https://wrds-web.wharton.upenn.edu/}
"USbonds"

#' Daily interest rates from 4/1/1954 to 28/12/2018
#' 
#' A dataset containing the daily 3 Month US Treasury Bill rate
#' 
#' @format A data frame with 16955 rows and 2 variables:
#' \describe{
#' \item{date}{vector of dates}
#' \item{rate}{daily annualised interest rate in percentages}
#' }
# "DTB3"

#' Results of the calculation in the vignette
#'
#' @format A list required to produce sample figures and table
# "vignette_data"
