library(GridR)

double<-function(x){return(2*x)}

grid.init(service="condor.local", localTmpDir=".tmp")
a<-function(s, p, q){return(s+p+q)}
grid.apply("y",a, c(0,1,2),1, c(100,200,300), wait=TRUE, check=TRUE, batch=c(1,3))

b<-function(s, p){return(mean(s)+p)}



grid.apply("y1",double,3, wait=TRUE)


foo <- rnorm(100)
grid.apply("ycheck", b,foo,5, wait=TRUE, check=TRUE) # if internal functions are used, its important to set check=FALSE, otherwise the codetools package returns an error

grid.apply("y2",double,3, wait=FALSE)
grid.waitForResult("y2")

grid.apply("y3", sum,1:5, wait=TRUE, check=FALSE) # if internal functions are used, its important to set check=FALSE, otherwise the codetools package returns an error

