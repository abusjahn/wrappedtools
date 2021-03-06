% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basefunctions.R
\name{bt}
\alias{bt}
\title{Add backticks to names or remove them}
\usage{
bt(x, remove = FALSE)
}
\arguments{
\item{x}{Names to add backtick to.}

\item{remove}{Option to remove existing backticks, default=FALSE.}
}
\value{
Character vector with backticks added.
}
\description{
\code{bt} adds leading and trailing backticks to make illegal variable names
usable. Optionally removes them.
}
\examples{
bt('name 1')

}
