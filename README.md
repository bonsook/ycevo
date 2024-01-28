
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ycevo

<!-- badges: start -->

[![R-CMD-check](https://github.com/bonsook/ycevo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bonsook/ycevo/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/ycevo)](https://CRAN.R-project.org/package=ycevo)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- [![metacran downloads](https://cranlogs.r-pkg.org/badges/ycevo)](https://cran.r-project.org/package=ycevo) -->
<!-- badges: end -->

The goal of `ycevo` is to provide means for the non-parametric
estimation of the discount function and yield curve of bonds.

If you use any data or code from the `ycevo` package CRAN release in a
publication, please use the following citation:

> Bonsoo Koo, and Yangzhuoran Fin Yang (2022). ycevo: Non-Parametric
> Estimation of the Yield Curve Evolution. R package version 0.1.2.
> <https://CRAN.R-project.org/package=ycevo>.

The package provides code used in Koo, La Vecchia, & Linton (2021).
Please use the following citation if you use any result from the paper.

> Koo, B., La Vecchia, D., & Linton, O. (2021). Estimation of a
> nonparametric model for bond prices from cross-section and time series
> information. Journal of Econometrics, 220(2), 562-588.

## Development cycle

The package is in active development and it have been experiencing
substantial changes. Since the existing forks have remained inactive for
several years, significant modifications have been implemented through
rebase. It is advisable for any existing forks to undergo the rebase
process.

    git remote add upstream https://github.com/bonsook/ycevo.git
    git fetch upstream
    git rebase upstream/master
    git push origin master --force

## Installation

You can install the **released** version of ycevo from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ycevo")
```

You can install the **development** version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bonsook/ycevo")
```

<!-- ## Usage -->

## License

This package is free and open source software, licensed under GPL-3
