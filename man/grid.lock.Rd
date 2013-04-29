\name{grid.lock}
\alias{grid.lock}
\title{ grid.lock}
\description{
  Adds a lock to variable varName
}
\usage{
grid.lock(varName)
}
\arguments{
  \item{varName}{ variable which should be locked }
}
\examples{
library("GridR")
grid.init(service="local")
x=1
y=2
grid.lock("x")
\dontrun{x}

}
\author{ Stefan Rueping }
\keyword{ programming }
