% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{generate_yield}
\alias{generate_yield}
\title{Generate a yield curve with cubic time evolution}
\usage{
generate_yield(
  max_qDate = 12,
  periods = 36,
  b0 = 0,
  b1 = 0.05,
  b2 = 2,
  t1 = 3,
  t2 = 500,
  linear = -0.55,
  quadratic = 0.55,
  cubic = -0.55
)
}
\arguments{
\item{max_qDate}{An integer giving the number of quotation dates to use in the data. Defaults to 12.}

\item{periods}{An integer giving the maximum number of time-to-maturity periods the yield curve is estimated for each quotation date. Defaults to 36}

\item{b0}{First term in yield curve equation, Defaults to 0. See \code{Details}.}

\item{b1}{Second term in yield curve equation, Defaults to 0.05. See \code{Details}.}

\item{b2}{Third term in yield curve equation, Defaults to 2. See \code{Details}.}

\item{t1}{Fourth term in yield curve equation, Defaults to 3. See \code{Details}.}

\item{t2}{Fifth term in yield curve equation, Defaults to 500. See \code{Details}.}

\item{linear}{Linear term in yield curve evolution, Defaults to -0.55. See \code{Details}.}

\item{quadratic}{Quadratic term in yield curve evolution. Defaults to 0.55. See \code{Details}.}

\item{cubic}{Cubic term in yield curve evolution. Defaults to -0.55. See \code{Details}.}
}
\value{
Matrix. Each column is a yield curve in a point in time (a quotation date). Each row is for a time-to-maturity.
For example, the number in the second column third row is the yield for the yield curve at the second quotation date,
for the third time-to-maturity ranking from shortest to longest. See \code{Details} for the equation to generate the yield curve.
See \code{Examples} for a example with the code to visually inspect the yield curves.
}
\description{
Generate a yield curve with cubic time evolution
}
\details{
Returns a matrix where each column corresponds to a yield curve at a different point in time.
The initial curve at time to maturity zero is estimated from the following equation
\deqn{Yield_{i, 0} = b_0 + b_1 * ((1 - \exp(-\tau_i / t_1)) / (\tau / t_1)) + b_2 * ((1 - \exp(-\tau_i / t_2)) / (\tau_i / t2) - \exp(-\tau_i / t_2))}
where \eqn{\tau_i} is the index of the time to maturity period. This defines the yield curve for the quotation date = 0.
The yield curve for quotation dates = 1, 2, ... , max_q_date multiplies this curve by the cubic equation,
\deqn{Yield_{i, t} = Yield_{i, 0} * (1 + linear * t + quadratic * t^2 + cubic * t^3)}
so the yield curve slowly changes over different quotation dates.
}
\examples{
out <- generate_yield()

# plots
library(tidyverse)
out <- data.frame(out)
colnames(out) <- 1:12
out <- mutate(out, time = 1:36)
out <- pivot_longer(out, -time, names_to = "qdate", values_to = "yield")
ggplot(out) +
  geom_line(aes(x=time, y=yield, color = qdate))

}
