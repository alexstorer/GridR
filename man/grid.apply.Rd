\name{grid.apply}
\alias{grid.apply}
\title{ grid.apply }
\description{
  grid.apply submits a function to another Host or Cluster, to execute it there.
}
\usage{
grid.apply(grid.input.Parameters.y=NULL,grid.input.Parameters.f=NULL,... ,wait=FALSE ,varlist=c(),plots=FALSE, run=1, check=TRUE, batch=NULL, javaSsh=FALSE)
}
\arguments{
  \item{grid.input.Parameters.y}{The variable in " " where to write the output if the job is finished }
  \item{grid.input.Parameters.f}{ which function should be executed remotely }
  \item{\dots}{ the parameters of the function}
  \item{wait}{ if wait=TRUE, R blocks until the Job is executed, otherwise Job starts in background}
  \item{varlist}{ if you know which variables or functions are needed to execute that function remotely, add them here if you do not want to use check=TRUE}
  \item{plots}{ is there a plot output of the function? Will be implemented in future}
  \item{run}{ please do not change, for internal use }
  \item{check}{ if check=TRUE, before submission it is checked if the function needs other variables or other functions to executed it remotely. If so, they are copied. If an intenal function like sum is applied, please use check=FALSE, otherwise codetools doesnt work }
  \item{batch}{ If you want the output of different parameter sets, you can add here which parameters are vectors and should be sweeped ie. c(1,3) then all combinations of parameter 1 and 3 are executed, only works with condor.ssh, remote.ssh and local modes}
  \item{javaSsh}{ if TRUE, ssh transmission will work on Linux Systems over a JavaSsh Library and not directly over ssh}
}
\details{
  Until the result is back, there is a lock to variable y
  
  Be careful: The variable \"grid.input.Parameters\" is used internally, so please do not use it in your code!
  
  To delete old local tmp files call \link{grid.consistency}
}
\author{ Malte Lohmeyer }

\examples{
#example to execute function a on a single remote host via ssh and save the result to x
library("GridR")
a<-function(s){Sys.sleep(s); return(s)}
grid.init(service="local", localTmpDir="GridRTmp/")
grid.apply("x",a, 3, wait=TRUE, check=TRUE)
x

#example for batch mode
library("GridR")
a<-function(s, p, q){return(s+p+q)}
grid.init(service="local", localTmpDir="GridRTmp/")
grid.apply("y",a, c(0,1,2),1, c(100,200,300), wait=TRUE, check=FALSE, batch=c(1,3))
#Here all combinations of (0,1,2), 1 and (100,200,300) are executed as parameters of function a. A List with all combinations is returned.
y

}
\keyword{ programming }
\seealso{
\code{\link{grid.init}}
}