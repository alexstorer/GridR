\name{grid.isLocked}
\alias{grid.isLocked}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ grid.isLocked}
\description{
  returns TRUE if varName is Locked, FALSE otherwise
}
\usage{
grid.isLocked(varName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varName}{ this variable will be checked }
}
\examples{
library("GridR")
grid.init(service="variableSharing")
x=1
y=2
grid.lock("x")
grid.isLocked("x")
grid.isLocked("y")
}
\author{ Malte Lohmeyer }
\keyword{ programming }
