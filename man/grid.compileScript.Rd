\name{grid.compileScript}
\alias{grid.compileScript}
\title{grid.compileScript}
\description{
  compiles an R script from the R history. All entered lines in the actual R Session are saved to path.
  If there is a parsing error, an error message is shown
}
\usage{
grid.compileScript(path)
}
\arguments{
  \item{path}{path of the output file}
  }
\author{ Malte Lohmeyer }

\examples{
#library("GridR")
#grid.init(service="local")
#a=1
#1
#grid.compileScript("test.R")
}
\keyword{ programming }