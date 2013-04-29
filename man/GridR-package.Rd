\name{GridR-package}
\alias{GridR}
\alias{Gridr}
\alias{gridR}
\alias{gridr}
\docType{package}
\title{
GridR executes a function on remote Hosts or Cluster.
}
\description{
GridR is an R-Package that submits R functions to execute them on another computer or cluster and it provides an interface to share functions and variables with other users.\cr 
Submission modes are using a web service, ssh or local, execution modes are condor, globus or using a single server.\cr
All needed functions and variables that are necessary to execute that function will be copied to the execution machine.\cr
}
\details{
\tabular{ll}{
Package: \tab GridR\cr
Type: \tab Package\cr
Version: \tab 0.9.1\cr
Date: \tab 2009-01-30\cr
License: \tab GPL-2\cr
}
With grid.init(...) GridR is initialized.
\cr
The specification of the configuration parameters can be performed with the help of a config file (that is read when the command grid.init() is performed) or at runtime by passing the parameters to the functions grid.init() and grid.apply(). \cr
The name of the config file is ".gridr.conf" or "gridr.conf" and it should be placed inside the user home directory or the directory from which R is executed.(see \link{grid.init} )\cr
\cr
Using grid.apply you can submit a function to be executed in the grid.(see \link{grid.apply})\cr
After execution the result will be copied back.

With grid.share you can share functions and variables with other users on your computer. If you have installed an nfs you can share them with remote users.(see \link{grid.share} )
 

}
\author{
Malte Lohmeyer
Dennis Wegener
Stefan Rueping
}
\keyword{ package }
\seealso{
\code{\link{grid.init}}
\code{\link{grid.apply}}
\code{\link{grid.share}}
\code{\link{grid.enableSharing}}
\code{\link{grid.disableSharing}}
\code{\link{grid.exit}}
\code{\link{grid.printJobs}}
\code{\link{grid.isLocked}}
\code{\link{grid.waitForResult}}
\code{\link{grid.consistency}}
\code{\link{grid.callback}}
\code{\link{grid.check}}
\code{\link{grid.cogMyproxy}}
\code{\link{grid.globusMyproxy}}
\code{\link{grid.proxyInit}}
\code{\link{grid.lock}}
\code{\link{grid.unlock}}

}
\examples{
a<-function(s){return(2*s)}
#define a function that will be executed remotely
library("GridR")
#load the gridR-Code
grid.init(service="local",debug=FALSE, localTmpDir="GridRTmp/")
#initializes gridR
grid.apply("x",a, 3, wait=TRUE)
#applies function `a` with parameter 3 and writes the result to variable x. until the function is executed, x has a lock.
}
