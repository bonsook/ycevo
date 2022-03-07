
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ycevo

<!-- badges: start -->

[![R-CMD-check](https://github.com/bonsook/ycevo/workflows/R-CMD-check/badge.svg)](https://github.com/bonsook/ycevo/actions)
<!-- badges: end -->

The goal of ycevo is to provide a range of functions to facilitate the
non-parametric estimation of the discount rate, and yield curve, of CRSP
Bond Data.

If you use any data or code from the `ycevo` package in a publication,
please use the following citation:

> Bonsoo Koo, Nathaniel Tomasetti, Kai-Yang Goh and Yangzhuoran Yang
> (2020). ycevo: Non-Parametric Estimation of the Yield Curve Evolution.
> R package version 1.0.0. <https://github.com/bonsook/ycevo>.

The package provides code used in Koo, La Vecchia, & Linton (2019).
Please use the following citation if you use any result from the paper.

> B. Koo, D. La Vecchia and O. Linton, Estimation of a nonparametric
> model for bond prices from cross-section and time series information.
> Journal of Econometrics (2020),
> <https://doi.org/10.1016/j.jeconom.2020.04.014>.

## Installation

<!-- You can install the released version of ycevo from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("ycevo") -->
<!-- ``` -->

The **stable** version on CRAN is coming soon.

The **development** version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bonsook/ycevo")
```

## Usage

``` r
library(ycevo)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.8
#> v tidyr   1.2.0     v stringr 1.4.0
#> v readr   2.1.2     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
# Twelve quotation dates
max_qDate <- 12
#Estimate a 36 period ahead yield curve for each qdate
periods <- 36

set.seed(1)
data <- simulate_data(max_qDate = max_qDate, periods = periods)

# grid values
xgrid <- (1:(max_qDate - 2))/(max_qDate - 1)
# bandwidth for each grid value
hx <- rep(0.5 /  max_qDate, max_qDate -2)

tau <- 1:periods
max_tumat <- max(data$tumat)
tau <- tau[1:min(which(tau >= max_tumat))]
tau <- tau/365# specified in years
ht <- rep(0.1, length(tau))

yield <- estimate_yield(
    data = data,
    xgrid = xgrid,
    hx = hx,
    tau = tau,
    ht = ht)

yield %>%
  mutate(xgrid = round(xgrid, 2)) %>%
  ggplot() + geom_line(aes(tau, yield)) +
  facet_wrap(~xgrid) 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
# Yield vs Quotation Dates (xgrid) for some time to maturities
select_tau <- c(3, 12, 18, 24)/365

yield %>%
  filter(tau %in% select_tau) %>%
  ggplot() + geom_line(aes(xgrid, yield)) +
  facet_wrap(~tau) 
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

## License

This package is free and open source software, licensed under GPL-3
