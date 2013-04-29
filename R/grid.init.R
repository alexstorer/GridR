# 	GridR package
#	Copyright (C) 2008 Fraunhofer Institute Intelligent Analysis and Information Systems IAIS, Dennis Wegener (dennis.wegener@iais.fraunhofer.de), Malte Lohmeyer (malte.lohmeyer@iais.fraunhofer.de), Stefan Rueping (stefan.rueping@iais.fraunhofer.de)  name of author
#		
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License Version 2
#	as published by the Free Software Foundation
#		
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#		
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

`grid.init` <-
		function(confFile=NULL, localTmpDir=NULL, verbose=TRUE, sshRemoteIp=NULL, sshUsername=NULL, sshRemoteDir=NULL, myProxyHost=NULL, myProxyUsername=NULL, credentialName=NULL, myProxyPwd=NULL, myProxyPort=NULL, service=NULL, sshKey=NULL, debug=FALSE, sharedDir=NULL, remoteRPath=NULL, schedulerIp=NULL, schedulerPort=NULL){
	#delete old values
	.grid$localDir=NULL
	.grid$ssh$ip=NULL
	.grid$ssh$username=NULL
	.grid$myProxyUsername=NULL
	.grid$pwd=NULL
	.grid$myproxyPort=NULL
	.grid$credentialName=NULL
	.grid$ssh$remotePath=NULL
	.grid$javaClientPath=NULL
	.grid$globusHost=NULL
	.grid$myProxyHost=NULL
	.grid$service=NULL
	.grid$cogDir=NULL
	.grid$ssh$key=NULL
	.grid$remoteRPath=NULL
	.grid$nfs$dir=NULL
	.grid$nfs$dir=NULL
	.grid$schedulerIp=NULL
	.grid$schedulerPort=NULL

    #load config file  
	if(!exists(".grid", inherits=TRUE)){
		cat("Error,no .grid var!")
		return(FALSE)
	}
	
	env=loadNamespace("GridR")
	unlockBinding(".grid", env)
	
	if(debug)
		.grid$debug=TRUE
	else
		.grid$debug=FALSE
	
	.grid$javaClientPath=NULL
	for(i in 1:length(.libPaths())){
		if(file.access(paste(.libPaths()[i],"/GridR/GridR/",sep=""))==0)
			.grid$javaClientPath=paste(.libPaths()[i],"/GridR/GridR/",sep="")
	}
	if(is.null(.grid$javaClientPath))
		cat("Error: cannot find the path where GridR is installed, thus all functions which need java code will not work\n")
	
	
	
	configPath=""
	if(!is.null(confFile) && file.access(confFile)==0){	
		configPath=	confFile
	}
	else
	{
		if(file.access("~/gridr.conf")==0) 
			configPath=	"~/gridr.conf"
		if(file.access("gridr.conf")==0) 
			configPath=	"gridr.conf"
	}
	if(configPath=="")
	{
		if(.grid$debug)
			cat("GridR is not using a config file\n")
	}
	else {
		if(.grid$debug)
			cat(paste("using config file: ",configPath, "\n", sep=""))
		
		#open config file
		config=scan(configPath, what=character(0), quiet=TRUE, sep="\n")
		if(length(config)<2)
		{
			print(paste("wrong config file ( #lines <2)", configPath))
			return(FALSE)
		}
		i=1
		while( i < length(config)){
			#scan
			
			#found a comment?
			if(substr(config[i],1,1)=="#"){
				i=i+1
				next
			}
			start=gregexpr("<", config[i])[[1]] #find all < 
			end=gregexpr(">", config[i])[[1]]   # find all >
			if(i==1) {
				if(gregexpr("<?xml",config[i])[[1]][1]!=-1){
					if(gregexpr("<GRIDR>",config[i+1])[[1]]==-1){
						cat("wrong config file( second line != <GRIDR>\n")
						return(FALSE)
					}
					else{
						i=i+2
						next
					}
				}
				else if(gregexpr("<GRIDR>",config[i])[[1]]==-1){
					cat("wrong config file( first line != <GRIDR>\n")
					return(FALSE)
				}
				else{
					i=i+1
					next
				}
			} else if(length(start)!=length(end) ||length(end)!=2) {#check if number of start and end tags are differen or if there are more than 2 tags per line
				if(length(end)==1 && gregexpr("</GRIDR>",config[i])[[1]]!=-1)
					break
				else
					cat("Syntax error in config file in line ", i, " wrong < and >\n")
			}
			#look if start and end tag are the same
			tag=substr(config[i],start[1]+1, end[1]-1) 
			str=substr(config[i],start[2]+2, end[2]-1)
			if(tag!=str) {
				cat("Syntax error in confog file in line ", i, " start and end tag are different\n")
				return(FALSE)
			}
			
			value=substr(config[i],end[1]+1, start[2]-1)
			#delete " " at the beginning
			if(nchar(value)>1 && substr(value,1,1)==" ")
				value=substr(value,2,nchar(value))
			#delete " " at the end
			if(nchar(value)>1 && substr(value,nchar(value),nchar(value))==" ")
				value=substr(value,nchar(value)-1,nchar(value)-1)
			
			if(.grid$debug)
				cat(paste("found Config File entry : ",tag, " with value ", value, "\n", sep=""))
			
			#	save readed values from config file to the right variable
			if(tag=="LOCALTMPDIR")
				.grid$localDir=gsub("\\\\","/",value)
			else if(tag=="SSHREMOTEIP")
				.grid$ssh$ip=value
			else if(tag=="SSHUSERNAME")
				.grid$ssh$username=value
			else if(tag=="MYPROXYUSERNAME")
				.grid$myProxyUsername=value
			else if(tag=="MYPROXYPWD")
				.grid$pwd=value
			else if(tag=="MYPROXYPORT")
				.grid$myproxyPort=value
			else if(tag=="CREDENTIALNAME")
				.grid$credentialName=value
			else if(tag=="SSHREMOTEDIR")
				.grid$ssh$remotePath=value
			else if(tag=="JAVACLIENTPATH")
				.grid$javaClientPath=value
			else if(tag=="GLOBUSEXECUTIONHOST")
				.grid$globusHost=value
			else if(tag=="MYPROXYHOST")
				.grid$myProxyHost=value
			else if(tag=="SERVICE")
				.grid$service=value
			else if(tag=="COGREMOTEDIR")
				.grid$cogDir=value
			else if(tag=="SSHKEY")
				.grid$ssh$key=value
			else if(tag=="REMOTERPATH")
				.grid$remoteRPath=value
			else if(tag=="NFSDIR")
				.grid$nfs$dir=gsub("\\\\","/",value)
			else if(tag=="SHAREDDIR")
				.grid$nfs$dir=gsub("\\\\","/",value)
			else if(tag=="SCHEDULERIP")
				.grid$schedulerIp=value
			else if(tag=="SCHEDULERPORT")
				.grid$schedulerPort=value
			
			
			i=i+1
		}
	}
	#save command line parameters to the right variable
	if(!is.null(service))
		.grid$service<- service
	if(!is.null(localTmpDir))
		.grid$localDir <- gsub("\\\\","/",localTmpDir)
	if(!is.null(sshRemoteIp))
		.grid$ssh$ip=sshRemoteIp
#  if(!is.null(condorUrl))
#	  .grid$condorUrl=condorUrl
	# if(!is.null(globusUrl))
#	  .grid$globusUrl=globusUrl
	if(!is.null(sshRemoteDir))
		.grid$ssh$remotePath=sshRemoteDir
#  if(!is.null(globusExecutionHost))
#	  .grid$globusHost=globusExecutionHost
	if(!is.null(sshUsername))
		.grid$ssh$username=sshUsername
	if(!is.null(myProxyHost))
		.grid$myProxyHost=myProxyHost
	if(!is.null(myProxyUsername))
		.grid$myProxyUsername=myProxyUsername
	if(!is.null(myProxyPwd))
		.grid$pwd=myProxyPwd
	if(!is.null(myProxyPort))
		.grid$myproxyPort=myProxyPort
	if(!is.null(credentialName))
		.grid$credentialName=credentialName
	if(!is.null(sshKey))
		.grid$ssh$key=sshKey
	if(!is.null(remoteRPath))
		.grid$remoteRPath=remoteRPath
	if(!is.null(sharedDir))
		.grid$nfs$dir=sharedDir
	if(!is.null(schedulerIp))
		.grid$schedulerIp=schedulerIp 
	if(!is.null(schedulerPort))
		.grid$schedulerPort=schedulerPort
	
	# if scheduler Ip is set, start GridR in SchedulerMode
	if(!is.null(.grid$schedulerIp)){
		if(!capabilities(what="sockets")){
			warning("Your R version doesn't support sockets, please install a newer one. Up to that, non-scheduler mode will be used")
		}else if(.grid$debug)
			cat("starting GridR in scheduler mode. Using scheduler ", .grid$schedulerIp, "\n")
		.grid$schedulerMode=TRUE
	}
	else{
		if(.grid$debug)
			cat("starting GridR without Scheduler\n")
		.grid$schedulerMode=FALSE
	}
	# if no scheduler Port is entered use default parameter:
	if(.grid$schedulerMode && is.null(.grid$schedulerPort))
		#TODO start at free port and save it here
		.grid$schedulerPort=4444


#check if all needed parameters are entered
if(is.null(.grid$service)){
	cat("service is not specified. Check Config File and Parameters\n wrong service, availible services are: local, condor.ssh, remote.ssh, globus.cog and variableSharing\n")
	return(FALSE)
}
if(!(.grid$service=="condor.ssh" ||.grid$service=="local" || .grid$service =="remote.ssh" || .grid$service=="globus.cog" || .grid$service=="globus.ssh" ||.grid$service=="variableSharing"))
{
	cat("wrong service, availible services are: local, condor.ssh, remote.ssh, globus.cog, globus.ssh(only scheduler Mode!) and variableSharing\n")
	return(FALSE)
}
err=FALSE
if(is.null(.grid$localDir) && .grid$service!="variableSharing"){
	cat("localTmpDir is not specified. Check config file and parameters.\n")
	err=TRUE
}
if(is.null(.grid$nfs$dir) ){
	if(.grid$debug)
		cat("GridR variableSharing will be disabled, because sharedDir is not specified. \n")
	.grid$nfs$run=FALSE
}
else
	.grid$nfs$run=TRUE

if (is.null(.grid$ssh$ip) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" || .grid$service=="globus.ssh" )) {
	cat("sshRemoteIp is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if(.grid$service=="globus.ssh" && !.grid$schedulerMode){
	cat("globus.ssh Mode is only in Scheduler Mode supported\n")
	err=TRUE
}
if (is.null(.grid$ssh$remotePath) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" || .grid$service=="globus.ssh")) {
	cat("sshRemoteDir is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if(is.null(.grid$globusHost) && ( .grid$service=="globus.cog" ) ) {
	cat("globusExecutionHost is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if(is.null(.grid$ssh$username) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" )){
	cat("sshUsername is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if(is.null(.grid$cogDir) && .grid$service=="globus.cog") {
	cat("cogRemoteDir is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if (is.null(.grid$ssh$key) && Sys.info()["sysname"]=="Windows" && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" || .grid$service=="globus.ssh" )) {
	cat("sshKey is not specified. Check Config File and Parameters\n")
	err=TRUE
}
if(.grid$service=="condor.ssh" && is.null(.grid$remoteRPath))
	.grid$remoteRPath="/usr/bin/R"

if(err)
	return(FALSE)

#add / to the end of dirs if not exists
if(!is.null(.grid$localDir) && substring(.grid$localDir,nchar(.grid$localDir)) != "/"){ .grid$localDir <- paste(.grid$localDir,"/",sep="") }
if(!is.null(.grid$nfs$dir) && substring(.grid$nfs$dir,nchar(.grid$nfs$dir)) != "/"){ .grid$nfs$dir <- paste(.grid$nfs$dir,"/",sep="") }
if(!is.null(.grid$cogDir) && substring(.grid$cogDir,nchar(.grid$cogDir)) != "/"){ .grid$cogDir <- paste(.grid$cogDir,"/",sep="") }
if(.grid$service=="condor.ssh" || .grid$service=="remote.ssh") {
	if(substring(.grid$ssh$remotePath,nchar(.grid$ssh$remotePath)) != "/"){ .grid$ssh$remotePath <- paste(.grid$ssh$remotePath,"/",sep="") }
}


#set absolute path of localDir
if(!is.null(.grid$localDir) && substr(.grid$localDir,1,1)!="/" && substr(.grid$localDir,2,3)!=":/")
	.grid$localDir = paste(getwd(), "/", .grid$localDir, sep="")
#check if these dirs exists
if(!is.null(.grid$localDir) && is.na(file.info(substr(.grid$localDir,1, nchar(.grid$localDir)-1))$"isdir"))
	dir.create(.grid$localDir)

nfsDirExists=file.info(substr(.grid$nfs$dir,1, nchar(.grid$nfs$dir)-1))$"isdir"
if(.grid$nfs$run && is.na(nfsDirExists))
	dir.create(.grid$nfs$dir)

#initialize .grid
.grid$uniqueName <- paste("grid",Sys.info()["nodename"],Sys.getpid(),gsub(" |:","-",perl=TRUE,as.character(Sys.time())),.grid$count,sep="-")
.grid$callback <- addTaskCallback(grid.callback)
if(.grid$nfs$run)
	.grid$shareCallback <- addTaskCallback(grid.shareCallback)

#start saving the history 
#grid.historyTaskCallback <- function(...){
#	.gridhistory = paste(.gridhistory, paste(list(...)[[1]], collapse=" "), "\n", sep="")
#	print(.gridhistory)
#	assign(".gridhistory", .gridhistory, loadNamespace("GridR"))
#	return(TRUE)
#}
#unlockBinding(".gridhistory", env)
#.grid$historyCallback = addTaskCallback(grid.historyTaskCallback)

.grid$verbose <- verbose

#init classpath separator for different operating systems
if(Sys.info()["sysname"]=="Linux"){
	.grid$classSeparator=":"
	.grid$system="linux"
}
else if(Sys.info()["sysname"]=="Darwin"){
	.grid$classSeparator=":"
	.grid$system="linux"
}
else if(Sys.info()["sysname"]=="Windows"){
	.grid$classSeparator=";"
	.grid$system="windows"
}
else{
	cat(paste("wrong Operating System, availible are: \"Windows\", \"Linux\" and \"Darvin\"\n your Operating system is: ",Sys.info()["sysname"] ))
	return(FALSE)
}

# add all jars in the GridR inst Path
files=dir(.grid$javaClientPath, pattern=".jar$", recursive=TRUE)
if(length(files)>0)
{
	for (i in 1:length(files)){
		Sys.setenv(CLASSPATH=paste(Sys.getenv("CLASSPATH"),.grid$classSeparator,.grid$javaClientPath, files[i],sep=""))
	}
}

if(.grid$schedulerMode){
	res = system(paste("java de.fhg.iais.kd.djm.ServerRunning ",.grid$schedulerIp , " ",.grid$schedulerPort, sep=""),intern=TRUE, wait=TRUE)
	if(length(res)>1 && .grid$debug)
		cat("Error: ", res[2], "\n")
	if(res[1]=="no"){
		cat("cannot connect to Scheduler: ", .grid$schedulerIp ,":", .grid$schedulerPort, "\n")
		return(FALSE)
	}
	
	#check if there are running jobs on scheduler
	
	#create connection to scheduler
	conn = socketConnection(host=.grid$schedulerIp, port=.grid$schedulerPort, blocking=TRUE)
	command=paste("<job>\n<mode>getJobs</mode>\n<username>",.grid$ssh$username,"</username>\n</job>",sep="")
	writeLines(command, conn)
	lines = readLines(conn)
	if(length(lines)>0){
		for( i in 1:length(lines)){
			if(nchar(lines[i])==0)
				next;
			# line contains  id paste(.grid$uniqueName, y, wait, run, plots, varlist, check, batch, .grid$service, sep="$$$", collapse=",")
			#separate id from rest
			begin=1
			while(begin < nchar(lines[i]) && substr(lines[i], begin, begin)!=" ")
				begin = begin+1
			if(begin == nchar(lines[i])){
				cat("Error, cannot receive Jobs from Scheduler, no jobID separated with \" \" from rest\n")
				return(FALSE)
			}
			
			jobId= as.numeric(substr(lines[i], 1, begin-1))
			lines[i] = substr(lines[i], begin+1, nchar(lines[i]))
			# now split rest with "$$$"
			entries = strsplit(lines[i], "$$$", fixed=TRUE)[[1]]
			if(length(entries)!=9){
				cat("Error, cannot receive Jobs from Scheduler, localVars has size !=9. size is",length(entries),"\n")
				return(FALSE)
			}
			uniqueName=entries[1]
			varName=entries[2]
			wait=as.logical(entries[3])
			run=as.numeric(entries[4])
			plots=as.logical(entries[5])
			varlist=c()
			if(nchar(entries[6])>0)
				eval(parse(text=paste("varlist=c(\"",gsub(",","\",\"",entries[6]),"\")" ,sep="")))
			check=as.logical(entries[7])
			batch=c()
			if(nchar(entries[8])>0)
				eval(parse(text=paste("batch=c(\"",gsub(",","\",\"",entries[8]) ,"\")",sep="")))
			service=entries[9]
			job <- list(name=uniqueName,var=varName,envir=NULL, sizes=list(), wait=wait, run=run, plots=plots, varlist= varlist, check=check,batch=batch, service=service, codeToolsOld="", id=jobId)
			
			#look if job is running locally
			localJob=-1
			if(length(.grid$gridJobs)>0)
			{
				for( k in 1:length(.grid$gridJobs))
					if(.grid$gridJobs[[k]]$id==jobId)
						localJob = k
			}
			if(localJob==-1)
			{
				if(exists(varName))
					cat("found a job on scheduler for variable", varName , "jobId",jobId,"overwrite that variable? (y/n)\n")
				else
					cat("found a job on scheduler for variable", varName , "jobId",jobId,"save it to this variable? (y/n)\n")
				input = scan(nlines=1, what=character(0), quiet=T)
				if(input!="y"){
					cat("please enter a new variable to overwrite\n")
					varName = scan(nlines=1, what=character(0), quiet=T)
					job$var=varName
				}
				#save this new job
				thisJob=length(.grid$gridJobs)+1
				.grid$gridJobs[[thisJob]]<- job
				
				#lock var (cannot call grid.lock, because then .grid will be overwritten and destroyed)
				if(varName %in% .grid$lock$varName){
					stop(paste("Object"+varName+"has lock"))
				}
				else{
					# do locking
					ex <- exists(varName)
					if(! ex){
						assign(varName,NULL,.GlobalEnv)
					}
					val <- eval(parse(text=varName))
					functionname <- paste(".gridlock.",varName,sep="")
					text <- paste(functionname,"<- function(...){
									grid.callback()
									if(!(length(list(...))==0 && any(get(\".grid\", loadNamespace(\"GridR\"))$changed==varName)))
									stop(paste(\"Object\",varName,\"has write lock from grid function\"))
									else
									{
									eval(parse(text=varName))
									}
									}")
					eval(parse(text=text))
					rm(list=varName,pos=1)
					makeActiveBinding(varName,eval(parse(text=functionname)),.GlobalEnv)
					
					i <- length(.grid$lock$varName)+1
					.grid$lock$varName[i] <- varName
					.grid$lock$writeLock[i] <- TRUE
					.grid$lock$value[[i]] <- val
					.grid$lock$exists[i] <- ex
				}
			}
		}
	}
  }
	#end schedulerMode
assign(".grid",.grid, env)
}

