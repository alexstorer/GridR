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

`grid.apply` <-
function(grid.input.Parameters.y=NULL,grid.input.Parameters.f=NULL,... ,wait=FALSE ,varlist=c(),plots=FALSE, run=1, check=TRUE, batch=NULL, javaSsh=FALSE){ 
	if(is.null(.grid$system)){
		cat("Error, Please run grid.init(...) first!\n")
		return(FALSE)
	}
	
	if(.grid$debug)
		debugStr="1"
	else
		debugStr="0"
	
	if(is.null(grid.input.Parameters.y)|| is.null(grid.input.Parameters.f)){
		cat("wrong usage of grid.apply: first parameter is the variable where to write the output, second is the function, followed by the parameter(s) of the function\n")
		return(FALSE)
	}
	wd=getwd()
	setwd(.grid$localDir)
	
if(grid.input.Parameters.y %in% .grid$lock$varName){
  cat(grid.input.Parameters.y, " has a lock: please use a different variable\n")
  return(FALSE)
}
if(!is.character(grid.input.Parameters.y)){
	cat("The parameter specified for the variable is not a character, please add a variable name in \"varname\" here\n")
	return(FALSE)
}
if(!is.function(grid.input.Parameters.f)){
	cat("The parameter specified for the function is not a function, please add a function here\n")
	return(FALSE)
}
if(!is.null(batch) &&!is.vector(batch)){
	cat("Wrong usage of \"batch\": Please add the Parameters which should be batched as a vector or list\n")
	return(FALSE)
}
if((.grid$service=="globus.cog" ) && !is.null(batch)){
	cat("Batch mode is not supported in globus mode!\n")
	return(FALSE)
}
if(.grid$service!="local" && is.null(.grid$ssh$key) && (javaSsh==TRUE || Sys.info()["sysname"]=="Windows")){
	cat("Please specify where to find sshKey in config file or grid.init \n")
	return(FALSE)
}		

# check if ssh library is available
if( javaSsh==TRUE || ( Sys.info()["sysname"]=="Windows") && (.grid$service=="remote.ssh" || .grid$service=="condor.ssh") ) {
	#trileadSSHname = dir(path=.grid$javaClientPath ,pattern="trilead-ssh2-build[0-9]+.jar", ignore.case=TRUE)
	trileadSSHname = dir(path=.grid$javaClientPath ,pattern="trilead-ssh2-build[0-9]+.jar")
	if (is.na(trileadSSHname[1])) {
		cat("Trilead SSH library not found\n")
		cat("please download trilead-ssh2-build212.jar or similar at http://www.trilead.com/Download/Trilead_SSH_for_Java/ \n")
		cat("and store it into the following directory after the installation of GridR:",.grid$javaClientPath,"\n")
		return(FALSE)
	} else {
		trileadSSHname = trileadSSHname[1]
	}
}

grid.input.Parameters.x <- list(...)

if(is.vector(batch)){
	for(i in 1:length(batch)){
		#print(paste("grid.input.Parameters[",i,"]",grid.input.Parameters[[i]],"\n"))
		if(!is.vector(grid.input.Parameters.x[[i]])){
			cat(paste("Parameter", i, "is not a vector, please change it\n"))
			return(FALSE)
		}
	}
}
  .grid$uniqueName <- paste("grid",Sys.info()["nodename"],Sys.getpid(),gsub(" |:","-",perl=TRUE,as.character(Sys.time())),.grid$count,sep="-")
  scriptName <- paste(.grid$uniqueName,"-script.R",sep="")
  remMainName <- paste(.grid$uniqueName,"-RemMainScript.R",sep="")
  yName <- paste(.grid$uniqueName,"-y.dat",sep="")
  remScriptName <- paste(.grid$uniqueName,"-Remscript.R",sep="")
  fName <- paste(.grid$uniqueName,"-fx",sep="")
  psName <- paste(.grid$uniqueName,"-plot.ps",sep="")
  errName<- paste(.grid$uniqueName,"-err",sep="")
  condorName<-paste(.grid$uniqueName,"-script.condor",sep="")

  
	noCondor=FALSE
	if(.grid$service!="condor.ssh")
		noCondor=TRUE
	#if batch mode is used, submit all parameter combinations to condor by inserting a remote function which does the distribution
  if(!is.null(batch)){
	  #(grid.input.Parameters.x, fName, yName, varlist, remScriptName){
	remoteRPath=.grid$remoteRPath
	  save(list=c("grid.batchFunction","grid.getBatchCmd", "scriptName", "fName","batch", "noCondor", "check", "yName", "remScriptName", "errName","remoteRPath", "condorName","grid.input.Parameters.x","grid.input.Parameters.f", "varlist",varlist),file=fName)
	  #cmd is the function ( ie. f(grid.input.Parameters.x[[1]])) which should be executed
	  cmd <- "grid.batchFunction(grid.input.Parameters.x, fName, yName, varlist, scriptName, remScriptName, errName, condorName, batch, check, noCondor, remoteRPath)"
  }
  else {
	  save(list=c("grid.input.Parameters.x","grid.input.Parameters.f",varlist),file=fName)
	  #cmd is the function ( ie. f(grid.input.Parameters.x[[1]])) which should be executed
	  cmd <- paste("grid.input.Parameters.f(",sep="")
	  if(length(grid.input.Parameters.x) > 0){
	    cmd <- paste(cmd,"grid.input.Parameters.x[[1]]",sep="")
	    if(length(grid.input.Parameters.x) > 1){
	      for(i in 2:length(grid.input.Parameters.x)){
	        cmd <- paste(cmd,",grid.input.Parameters.x[[",i,"]]",sep="")
	      }
	    }
	  }
	  cmd <- paste(cmd,")",sep="")
  }
  #if(check) all neccesarry variables and functions available? 
if(check){
	  tmp=varlist
	  varlist = grid.check(grid.input.Parameters.f,grid.input.Parameters.x,varlist, fName, intern=TRUE)
	  if(length(varlist)!=length(tmp)){
	  	#resave params because varlist has changed
	  	if(!is.null(batch))
			  save(list=c("grid.batchFunction","grid.getBatchCmd","fName","batch","noCondor", "check", "yName", "remScriptName", "scriptName", "errName","remoteRPath", "condorName","grid.input.Parameters.x","grid.input.Parameters.f", "varlist",varlist),file=fName)
	  	else
			  save(list=c("grid.input.Parameters.x","grid.input.Parameters.f",varlist),file=fName)
	  }
  }
  
  newGrid <- .grid
  newGrid$count <- newGrid$count+1
  jobs <- .grid$gridJobs
  id=-1
  job <- list(name=.grid$uniqueName,var=grid.input.Parameters.y,envir=environment(), sizes=list(), wait=wait, run=run, plots=plots, varlist= varlist, check=check,batch=batch, service=.grid$service, codeToolsOld="", id=id)
  jobStr =paste(.grid$uniqueName, grid.input.Parameters.y, wait, run, plots, paste(varlist, collapse=","), check, paste(batch, collapse=","), .grid$service, sep="$$$")
  thisJob=length(jobs)+1
  jobs[[thisJob]]<- job 
  newGrid$gridJobs <- jobs
  .grid=newGrid  ##ok here???
  assign(".grid",newGrid, loadNamespace("GridR"))
  
  if(.grid$schedulerMode)
  #create connection to scheduler
	conn = socketConnection(host=.grid$schedulerIp, port=.grid$schedulerPort, blocking=TRUE)
  
  if(wait)
	  sBlock="true"
  else
	  sBlock="false"
  
#Webservice modes:

########################################### local #############################################
if(.grid$service=="local")
{
	grid.makeSshAndCondorFiles(plots, yName, psName, fName, scriptName, NULL, NULL, varlist, cmd, TRUE, check)
	
	if(wait) {
		if(.grid$debug)
			cat("starting local mode\n")
		system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla ", scriptName, sep=""))
		grid.callback()
	}
	else
	{
		if(.grid$debug)
			cat("starting local mode\n")
		grid.lock(grid.input.Parameters.y)
		system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla ", scriptName, sep=""), wait=FALSE)
		
	}
}

##########################non-scheduler modes:
########################################### remote.ssh #############################################
else if(.grid$service=="remote.ssh" && !.grid$schedulerMode)
{
	grid.makeSshAndCondorFiles(plots, yName, psName, fName, scriptName, NULL, NULL, varlist, cmd, TRUE, check)
	
# make remote dir and copy files
if(.grid$system=="linux" && javaSsh==FALSE){
	system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"mkdir -p ",.grid$ssh$remotePath,"\"", sep=""), ignore.stderr=TRUE)
	err=system(paste("scp -B ",scriptName, " ", fName," ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath, " 2>&1", sep=""), intern=TRUE)
	if(length(err)!=0) {
		print(paste("Error, cannot copy files to remote host\n", err))
		return(FALSE)
	}	
	if(wait) {
		if(.grid$debug)
			cat("starting remote.ssh without JavaSsh\n")
		# start remote script and copy file back
		system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))
		err=system(paste("scp -B ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath,yName," ", yName, " 2>&1", sep=""),intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy result file from remote host\n", err))
			return(FALSE)
		}
		err=system(paste("scp -B ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath,scriptName,"out ", scriptName, "out 2>&1", sep=""),intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy script.Rout file from remote host\n", err))
			return(FALSE)
		}
		grid.callback()
		#delete remote files
		if(!.grid$debug)
			system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && rm ",.grid$uniqueName,"*\"",sep=""),intern=TRUE)
	}
	else
	{
		if(.grid$debug)
			cat("starting remote.ssh without JavaSsh\n")
		grid.lock(grid.input.Parameters.y)
		#start remote script
		system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))#, intern=TRUE)
		grid.waitSshResultFile(yName, paste(scriptName, "out", sep=""))
		system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla --slave ",paste(.grid$uniqueName, "-waitForReturn.R",sep=""), " &", sep=""))
	}
}
else{#windows
	if(wait) {
		if(.grid$debug)
			cat("starting remote.ssh with JavaSsh\n")
		return=try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"","-", "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName, "\" \"",debugStr, "\"",sep="")))
		if(length(return)>1)
			print(return)
		grid.callback()
	}
	else{
		if(.grid$debug)
			cat("starting remote.ssh with JavaSsh\n")
		grid.lock(grid.input.Parameters.y)
		try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"","-", "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName, "\" \"",debugStr, "\"",sep=""), wait=FALSE))
		}
	}
	
}
	
########################################### Condor.ssh #############################################
else if(.grid$service=="condor.ssh" && is.null(batch) && !.grid$schedulerMode)
{
	grid.makeSshAndCondorFiles(plots, yName, psName, fName, remScriptName, scriptName, paste(scriptName, "out",sep=""), varlist, cmd, FALSE, check)
	if(.grid$system=="linux" && javaSsh==FALSE){
		# make remote dir and copy files
		system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"mkdir -p ",.grid$ssh$remotePath,"\"", sep=""), ignore.stderr=TRUE)
		err=system(paste("scp -B ",remScriptName, " ",scriptName, " ", fName," ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath, " 2>&1",sep=""), intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy files to remote host\n", err, sep=""))
			return(FALSE)
		}	
		if(wait) {
			if(.grid$debug)
				cat("starting condor.ssh without JavaSsh\n")
			# start remote script and copy file back
			system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))
			err=system(paste("scp -B ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath,yName," ", yName," 2>&1", sep=""),intern=TRUE)
			if(length(err)!=0) {
				print(paste("Error, cannot copy files from remote host\n", err))
				return(FALSE)
			}	
			grid.callback()
			#delete remote files
			if(!.grid$debug)
				system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && rm ",.grid$uniqueName,"*\"",sep=""),intern=TRUE)
		}
		else
		{
			if(.grid$debug)
				cat("starting condor.ssh without JavaSsh\n")
			grid.lock(grid.input.Parameters.y)
			#start remote script
			system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))#, intern=TRUE)
			grid.waitSshResultFile(yName, paste(scriptName, "out", sep=""))
			system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla --slave ",paste(.grid$uniqueName, "-waitForReturn.R",sep=""), " &", sep=""))
		}
	}
	else{#Windows
		if(wait) {
			if(.grid$debug)
				cat("starting condor.ssh with JavaSsh\n")
			return=try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"",remScriptName, "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName,"\" \"",debugStr, "\"",sep="")))	
			if(length(return)>1)
				print(return)
			grid.callback()
		}
		else{
			if(.grid$debug)
				cat("starting condor.ssh with JavaSsh\n")
			grid.lock(grid.input.Parameters.y)
			try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"",remScriptName, "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName, "\" \"",debugStr, "\"",sep=""), wait=FALSE))
		}
		
	}
}
# ssh batch mode
else if(.grid$service=="condor.ssh" && !is.null(batch) && !.grid$schedulerMode)
{
	#grid.makeSshAndCondorFiles(plots, yName, psName, fName, remScriptName, remMainName,paste(remMainName, "out",sep=""), varlist, cmd, FALSE, batch=TRUE)
	grid.makeRemRFile(plots, scriptName, psName, varlist, cmd, check, outputFile=yName, remLibFilename=fName)
	
	if(.grid$system=="linux" && javaSsh==FALSE){	
		# make remote dir and copy files
		system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"mkdir -p ",.grid$ssh$remotePath,"\"", sep=""), ignore.stderr=TRUE)
		err=system(paste("scp -B ",scriptName, " ", fName," ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath, " 2>&1",sep=""), intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy files to remote host\n", err))
			return(FALSE)
		}	
		if(wait) {
			# start remote script and copy file back
			system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))
			err=system(paste("scp -B ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath,yName," ", yName, " 2>&1", sep=""),intern=TRUE)
			if(length(err)!=0) {
				print(paste("Error, cannot copy files from remote host\n", err))
				return(FALSE)
			}	
			grid.callback()
			#delete remote files
			if(!.grid$debug)
				system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && rm ",.grid$uniqueName,"*\"",sep=""),intern=TRUE)
		}
		else
		{
			grid.lock(grid.input.Parameters.y)
			#start remote script
			system(paste("ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \"cd ",.grid$ssh$remotePath," && ",.grid$remoteRPath," CMD BATCH --vanilla ", scriptName,"\"", sep=""))#, intern=TRUE)
			grid.waitSshResultFile(yName, paste(scriptName, "out", sep=""))
			system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla --slave ",paste(.grid$uniqueName, "-waitForReturn.R",sep=""), " &", sep=""))
		}
	}
	else{#windows
		if(wait) {
			return=try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"",NULL, "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName, "\" \"",debugStr, "\"",sep="")))	
			if(length(return)>1)
				print(return)
			grid.callback()
		}
		else{
			grid.lock(grid.input.Parameters.y)
			try(system(paste("java -classpath \"",.grid$javaClientPath, "\"", .grid$classSeparator, "\"", .grid$javaClientPath, trileadSSHname, "\" ", "MySSh ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",scriptName, "\" \"",NULL, "\" \"",fName, "\" \"",yName, "\" \"", .grid$uniqueName, "\" \"",debugStr, "\"",sep=""), wait=FALSE))
		}
	}
}
########################################### Globus.cog #############################################
else if(.grid$service=="globus.cog")
{
	#write remote R file
	grid.makeRemRFile(plots, remScriptName, psName, varlist, cmd, check, yName, fName)
	#write locak R file which copies files and executes the job
	grid.makeCogRFile(scriptName,remScriptName, fName, yName)
	
	if(wait){
		system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla --slave \"",scriptName,"\"", sep=""))
		.grid$tmp=grid.callback()
		
	}
	else{
		grid.lock(grid.input.Parameters.y)
		system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla --slave \"",scriptName, "\"", sep=""),wait=FALSE)
	}
}
#########################scheduler modes:
else 
{
	grid.makeSshAndCondorFiles(plots, yName, psName, fName, scriptName, NULL, NULL, varlist, cmd, TRUE, check)
	
	# make remote dir and copy files
	if(.grid$system=="linux" && javaSsh==FALSE){
		system(paste("ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \"mkdir -p ",.grid$ssh$remotePath,"\"", sep=""), wait=FALSE, ignore.stderr=TRUE)
		##copy in background
		err=system(paste("scp -B ",scriptName, " ", fName," ",.grid$ssh$username,"@", .grid$ssh$ip,":",.grid$ssh$remotePath, " 2>&1", sep=""), wait=FALSE, intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy files to remote host\n", err))
			return(FALSE)
		}
		if(.grid$debug)
			cat("starting",.grid$service,"without JavaSsh\n")
	}
	else{#windows
		system(paste("java de.fhg.iais.kd.gridr.interfaces.SshUpload ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , .grid$ssh$remotePath, "\" \"",yName, "\" \"",scriptName, "\" \"",fName, "\"",sep=""), wait=FALSE)
		if(.grid$debug)
			cat("starting remote.ssh",.grid$service," with JavaSsh\n")
	}
	
	#submit to scheduler
	grid.schedulerExecFile(scriptName, jobStr, sBlock, conn, fName, yName, paste(scriptName, "out",sep=""))
	recId=readLines(conn)
	if(any(!is.na(pmatch("Error", recId))))
		cat(recId, "\n")
	#save job id
	newgrid = .grid
	newgrid$gridJobs[[thisJob]]$id =recId[1]
	#cat(newgrid$gridJobs[[thisJob]]$id, " job: ", thisJob, "\n")
	assign(".grid",newgrid, loadNamespace("GridR"))
	
	if(wait)
		grid.callback()
	else
		grid.lock(grid.input.Parameters.y)	
}


setwd(wd)
}


`grid.schedulerExecFile` <-function(scriptName, jobStr, sBlock, conn, fName,   ...)
{
	executable = "R"
	if(.grid$service=="globus.ssh"){
		sMode ="execGlobusJob"
		executable = .grid$remoteRPath
	}
	else if(.grid$service=="condor.ssh"){
		sMode="execCondorJob"
		executable = .grid$remoteRPath
	}
	else if(.grid$service=="remote.ssh")
		sMode="execFile"
	else
		cat("wrong mode\n")
	outputFiles = list(...)
	command=paste("<job>\n<mode>",sMode,"</mode>\n<username>",.grid$ssh$username,"</username>\n<executable>", executable,"</executable>\n<arguments> CMD BATCH --vanilla ",
			scriptName,"</arguments> \n <remoteDir>",.grid$ssh$remotePath,"</remoteDir>\n<execIp>",.grid$ssh$ip,"</execIp>\n",sep="")
	for(i in 1:length(outputFiles))
		command=paste(command, "<outputFile>",outputFiles[[i]],"</outputFile>\n",sep="")
	command=paste(command, "<inputFile>",scriptName,":",file.info(paste(.grid$localDir,scriptName,sep=""))$size, "</inputFile>\n",
			"<inputFile>",fName,":",file.info(paste(.grid$localDir,fName,sep=""))$size, "</inputFile>\n",sep="")
	command=paste(command,"<block>",sBlock,"</block>\n<localVar>", jobStr, "</localVar>\n</job>",sep="")
	writeLines(command, conn)
}

