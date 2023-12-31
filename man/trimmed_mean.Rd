% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trimmed_mean.R
\name{trimmed_mean}
\alias{trimmed_mean}
\title{Calculate a trimmed mean for a numeric vector}
\usage{
trimmed_mean(x, l, s)
}
\arguments{
\item{x}{a numeric vector}

\item{l}{the l largest values}

\item{s}{the s smallerst values}
}
\value{
The trimmed mean of the numeric vector
}
\description{
Calculate a trimmed mean for a numeric vector
}
\examples{
trimmed_mean(c(1, 7, 3, 2, 5, 0.5, 9, 10), 1, 2)
}
