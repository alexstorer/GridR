library(GridR)

double<-function(x){return(2*x)}

grid.init(service="remote.ssh", localTmpDir="/tmp", sshRemoteIp="grid-node4", sshRemoteDir="grid", sshUsername="mlohmeyer")
grid.apply("y1",double,3, wait=TRUE)

grid.apply("y2",double,3, wait=FALSE)
grid.waitForResult("y2")
