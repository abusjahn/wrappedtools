% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptives.R
\name{se_median}
\alias{se_median}
\title{Compute standard error of median}
\usage{
se_median(x)
}
\arguments{
\item{x}{Data for computation.}
}
\value{
numeric vector with SE Median.
}
\description{
\code{se_median} is based on \code{\link{mad}}/square root(n)
(Deprecated, please see \link{medianse}, which is the same but named more consistently)
}
\examples{
# basic usage of se_median
\dontrun{
se_median(x = mtcars$wt)
}
}
