
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ycevo

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/FinYang/ycevo.svg?branch=master)](https://travis-ci.org/FinYang/ycevo)
<!-- badges: end -->

The goal of ycevo is to provide a range of functions to facilitate the
non-parametric estimation of the discount rate, and yield curve, of CRSP
Bond Data.

If you use any data or code from the `ycevo` package in a publication,
please use the following citation:

> Bonsoo Koo, Nathaniel Tomasetti, Kai-Yang Goh and Yangzhuoran Yang
> (2019). ycevo: Non-Parametric Estimation of the Yield Curve Evolution.
> R package version 1.0.0. <https://github.com/bonsook/ycevo>.

The package provides code used in Koo, La Vecchia, & Linton (2019).
Please use the following citation if you use any result from the paper.

> Koo, B., La Vecchia, D., & Linton, O. B. (2019). Estimation of a
> Nonparametric model for Bond Prices from Cross-section and Time series
> Information. Available at
SSRN3341344.

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
#> -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --
#> v ggplot2 3.2.1     v purrr   0.3.2
#> v tibble  2.1.3     v dplyr   0.8.3
#> v tidyr   1.0.0     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.4.0
#> -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(lubridate)# Twelve quotation dates
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
max_qDate <- 12
#Estimate a 36 period ahead yield curve for each qdate
periods <- 36

set.seed(1)
data <- simulate_data(max_qDate = max_qDate, periods = periods)
glimpse(data)
#> Observations: 5,616
#> Variables: 7
#> $ qdate     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
#> $ crspid    <fct> 1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9...
#> $ mid.price <dbl> 95.80754, 91.63284, 90.45671, 89.50187, 89.50187, 93...
#> $ tupq      <dbl> 1, 2, 3, 4, 1, 5, 2, 6, 3, 1, 7, 4, 5, 2, 8, 9, 3, 6...
#> $ pdint     <dbl> 101, 101, 101, 101, 1, 105, 5, 101, 1, 3, 103, 3, 1,...
#> $ tumat     <dbl> 1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9...
#> $ accint    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
# grid values
ugrid <- (1:(max_qDate - 2))/(max_qDate - 1)
# bandwidth for each grid value
hu <- rep(0.5 /  max_qDate, max_qDate -2)

qgrid <- 1:periods
max_tumat <- max(data$tumat)
qgrid <- qgrid[1:min(which(qgrid >= max_tumat))]
hq <- rep(0.1, length(qgrid))

xgrid <- qgrid
hx <- hq

yield <- estimate_yield(
    data = data,
    ugrid = ugrid,
    hu = hu,
    xgrid = xgrid,
    hx = hx,
    qgrid = qgrid,
    hq = hq,
    units = 1)
glimpse(yield)
#> Observations: 360
#> Variables: 4
#> $ discount <dbl> 0.9942668, 0.9465143, 0.9297756, 0.9118482, 0.8813457...
#> $ ug       <dbl> 0.09090909, 0.09090909, 0.09090909, 0.09090909, 0.090...
#> $ qg       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16...
#> $ yield    <dbl> 0.005749703, 0.027484603, 0.024270673, 0.023070440, 0...

# Yield vs Time (qgrid)
yield %>%
  mutate(ug = round(ug, 2)) %>%
  ggplot() + geom_line(aes(qg, yield)) +
  facet_wrap(~ug) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

# Yield vs Quotation Dates (ugrid) for some time to maturities

select_qgrid <- c(3, 12, 24, 36)

yield %>%
  filter(qg %in% select_qgrid) %>%
  ggplot() + geom_line(aes(ug, yield)) +
  facet_wrap(~qg) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

## License

This package is free and open source software, licensed under GPL-3
