% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_choices}
\alias{get_choices}
\title{Get Choices}
\usage{
get_choices(choices)
}
\arguments{
\item{choices}{An object that contains choices.}
}
\value{
A vector of choices.
}
\description{
This function returns choices based on the class of the input.
If the input is of class \code{delayed_data}, it returns the \code{subset} of the input.
Otherwise, it returns the input as is.
}
\keyword{internal}