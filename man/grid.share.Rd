\name{grid.share}
\alias{grid.share}
\title{ grid.share}
\description{
  grid.share shares functions and variables with other users on your computer. If an nfs is installed, they can be shared them with remote users.
}
\usage{
grid.share(varName)
}
\arguments{
  \item{varName}{The variable or functionname in " " which should be shared}
  }
\details{
If this function is executed, varName is stored to sharedDir which must be specified in the config file or the grid.init function.

If grid.init is called, a taskCallback is added which checks each time a command is entered to R if a new function or variable is stored to the sharedDir. If so it is loaded into R.

Be Careful: it overrides the old value automatically! 
}
\author{ Malte Lohmeyer }
\keyword{ programming }
\examples{
library("GridR")
grid.init(service="variableSharing", sharedDir="GridRTmp/share")
x=2
grid.share("x")
}