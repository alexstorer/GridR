\name{grid.unlock}
\alias{grid.unlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ grid.unlock}
\description{
  unlocks the specified variable
}
\usage{
grid.unlock(varName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varName}{ this variable will be unlocked }
}
\examples{
library("GridR")
grid.init(service="local")
x=1
grid.lock("x")
grid.unlock("x")
}

\author{ Stefan Rueping }
\keyword{ programming }
