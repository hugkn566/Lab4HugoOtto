% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.linreg}
\alias{summary.linreg}
\title{Summary method for linreg objects}
\usage{
\method{summary}{linreg}(object, ...)
}
\arguments{
\item{object}{an object of class "linreg", generated by a call to \code{\link{linreg}}.}

\item{...}{additional arguments.}
}
\value{
A list of summary statistics from a linear regression model, \code{object}, of the class "linreg".
}
\description{
\code{summary} method for objects of the class "linreg".
}
\examples{
linreg_obj <- linreg(Petal.Length~Species, datasets::iris)
summary(linreg_obj)
}
