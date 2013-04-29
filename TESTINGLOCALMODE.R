library(GridR)
grid.init()

double<-function(x){return(2*x)}

grid.init(service="local", localTmpDir="/tmp")
grid.apply("y1",double,3, wait=TRUE)

grid.apply("y2",double,3, wait=FALSE)
grid.waitForResult("y2")


grid.apply("y3", sum,1:5, wait=TRUE, check=FALSE) # if internal functions are used, its important to set check=FALSE, otherwise the codetools package returns an error
