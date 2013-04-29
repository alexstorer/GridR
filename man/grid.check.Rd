\name{grid.check}
\alias{grid.check}
\title{ grid.check }
\description{
  checks if varlist contains all variables and functions to execute function f. If not the missing variables are returned
}
\usage{
grid.check(grid.input.Parameters.f,x="",varlist=c(), fName="", intern=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid.input.Parameters.f}{ which functions should be checked}
  \item{x}{ for internal use}
  \item{varlist}{ a vector which contains needed variables and functions to execute f}
  \item{fName}{ for internal use}
  \item{intern}{with intern=FALSE a human output is printed, otherwise null or the missing variables are returned  }
}
\author{ Malte Lohmeyer}
\keyword{programming}
