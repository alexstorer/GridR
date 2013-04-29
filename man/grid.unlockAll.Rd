\name{grid.unlockAll}
\alias{grid.unlockAll}
\title{ grid.unlockAll}
\description{
  unlocks all locked variables
}
\usage{
grid.unlockAll()
}
\examples{
library("GridR")
grid.init(service="local")
x=1
y=1
grid.lock("x")
grid.lock("y")
grid.unlockAll()
}

\author{ Stefan Rueping }
\keyword{ programming }
