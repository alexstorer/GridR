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

`grid.callback` <-
function(...){
	if(is.null(.grid$schedulerMode))
	{
		cat("please run grid.init(...) first\n")
		return(FALSE)
	}
  # see if something is to do (grid)
  if(.grid$schedulerMode){
	#ckeck for ready jobs on scheduler
	conn = socketConnection(host=.grid$schedulerIp, port=.grid$schedulerPort, blocking=TRUE)
	command=paste("<job>\n<mode>checkAllResults</mode>\n<username>",.grid$ssh$username,"</username>\n</job>\n",sep="")
	writeLines(command, conn)
	outFiles = readLines(conn)
	idsToDelete=vector()
	if(!length(outFiles)==0)
	{
		for(i in 1:length(outFiles)){
	#		cat(outFiles[i],"\n")
			if(substr(outFiles[i], 1,22)=="ERROR: Job had Error: " || substr(outFiles[i], 1,15)=="Job had Error: "){
				cat(outFiles[i], "\n")
				# find jobID at the end of the string
				begin=nchar(outFiles[i])
				while(begin > 0 && substr(outFiles[i],begin,begin)!=" ")
					begin=begin-1
				jobIDs = substring(outFiles[i], begin+1, nchar(outFiles))
				jobID = as.integer(jobIDs)
		
				if(!is.na(jobID))
				{
					idsToDelete[length(idsToDelete)+1] =jobID
	
					# find local job number
					if(length(.grid$gridJobs)>0)
					{
						localJob=-1
						for( k in 1:length(.grid$gridJobs))
							if(.grid$gridJobs[[k]]$id==jobID)
								localJob = k
						if(localJob!=-1)
						{
							varName = .grid$gridJobs[[localJob]]$var
							#unlock 
							if(varName %in% .grid$lock$varName){
								i <- which(.grid$lock$varName == varName,arr.ind=TRUE)[1]
								# remove lock
								rm(list=varName,pos=1)
								if(.grid$lock$exists[i]){
									assign(varName,.grid$lock$value[[i]],.GlobalEnv)
								}
								ii <- setdiff(1:length(.grid$lock$varName),i)
								.grid$lock$varName <- .grid$lock$varName[ii]
								.grid$lock$writeLock <- .grid$lock$writeLock[ii]
								.grid$lock$value <- .grid$lock$value[ii]
								.grid$lock$exists <- .grid$lock$exists[ii]
							}
							#delete job
			
							if(!.grid$debug)
								unlink(paste(.grid$localDir,.grid$gridJobs[[localJob]]$name,"-*",sep=""))
							.grid$gridJobs <- .grid$gridJobs[setdiff(1:length(.grid$gridJobs),localJob)]
								next
						}
						else
							cat("error, no local job found for jobID", jobID, "\n")
					}
				}
				else
					cat("error, cannot parse JobId of the error String, thus files of the job cannot be deleted\n")
			}
			else if(substr(outFiles[i], 1,5)=="ERROR" || substr(outFiles[i], 1,5)=="Error")
			{
				cat(outFiles[i], "\n")
				next
			}
			else
			{
				nameAndSizeAndId = grid.copyFile(outFiles[i])
				if(length(.grid$gridJobs)!=0)
					for(j in 1:length(.grid$gridJobs))
					{
						if(!is.na(pmatch(.grid$gridJobs[[j]]$name,nameAndSizeAndId[1] )))
						{	#found right job
							.grid$gridJobs[[j]]$id=nameAndSizeAndId[3]
							.grid$gridJobs[[j]]$sizes[[length(.grid$gridJobs[[j]]$sizes)+1]]=nameAndSizeAndId
							#print(.grid$gridJobs[[j]]$sizes)
							#mark id to delete job later
							idsToDelete[length(idsToDelete)+1] = nameAndSizeAndId[3]
						}
					}
			}
		}
	}
	close(conn)
	if(length(idsToDelete)>0)
	{
		ids =unique(idsToDelete)
		for(i in 1:length(ids))
		{
			conn = socketConnection(host=.grid$schedulerIp, port=.grid$schedulerPort, blocking=TRUE)
			mode="";
			if(.grid$debug)
				mode = "deleteJob"
			else
				mode = "deleteFiles"
			command=paste("<job>\n<mode>",mode,"</mode>\n<username>",.grid$ssh$username,"</username>\n<id>",ids[i],"</id>\n</job>\n",sep="")
			writeLines(command, conn)
			close(conn)
		}
	}
  }

  reapply=FALSE			#if it is set to TRUE the job will be reapplied
  del=TRUE
  .grid$changed=NULL #changes if a job is finished this run
  grid.input.Parameters=NULL
  grid.input.Parameters.x=NULL
  jobs <- .grid$gridJobs
  i <- 1
  while(i <= length(jobs)){
      filename <- paste(.grid$localDir,jobs[[i]]$name,"-y.dat",sep="")
      remOutput= paste(jobs[[i]]$name,"-y.dat",sep="")
	  
   if(file.exists(filename)){
	   if(file.info(filename)$size==0){
 	     Sys.sleep(1)
	 }
   if(.grid$schedulerMode){
    #get filesize 
		size=-1
		jobId=-1
		if(length(jobs[[i]]$sizes)!=0){
			for(k in 1:length(jobs[[i]]$sizes)){
				if(length(jobs[[i]]$sizes[[k]])==3 && !is.na(pmatch(remOutput, jobs[[i]]$sizes[[k]][1])))
					{
						size=jobs[[i]]$sizes[[k]][2]
						jobId=jobs[[i]]$sizes[[k]][3]
					}
					
			}
		}
		if(size!=-1)
		{
			if(file.info(filename)$size!=size){
				if(.grid$debug)
 	    			cat("waiting for file: ", remOutput, "\n")
				next;
			}
	   }
	}
	  #unlock jobs[[i]]$var
	varName=jobs[[i]]$var
	if(varName %in% .grid$lock$varName){
		i <- which(.grid$lock$varName == varName,arr.ind=TRUE)[1]
		# remove lock
		rm(list=varName,pos=1)
		if(.grid$lock$exists[i]){
			assign(varName,.grid$lock$value[[i]],.GlobalEnv)
		}
		ii <- setdiff(1:length(.grid$lock$varName),i)
		.grid$lock$varName <- .grid$lock$varName[ii]
		.grid$lock$writeLock <- .grid$lock$writeLock[ii]
		.grid$lock$value <- .grid$lock$value[ii]
		.grid$lock$exists <- .grid$lock$exists[ii]
	}
	 errOcc=FALSE
      warn=getOption("warn")
	  options(warn=-1)
	#try to load the returned file. if there is a error, then the remote task had an error. the error is printed to this file
  	errLoad=try(load(file=filename), silent=TRUE)
	options(warn=warn)
	if(inherits(errLoad, "try-error")) {
		if(file.info(filename)$size==0){
			cat(paste("Grid job had error, see ", .grid$localDir,jobs[[i]]$name, "-script.Rout\n", sep=""))
			grid.unlock(.grid$gridJobs[[i]]$var)
			del=FALSE
			errOcc=TRUE
		}
		else {
			res = scan(file=filename, what=character(0), sep="\n", quiet=TRUE )
			warn=getOption("warn")
			options(warn=-1)
			if(length(res)==1 && file.exists(res)) { # no error, webservice response, load that file. res = path to file
		#		cat(res, "\n")
				load(res)
			#	unlink(res)
			}
			else {
				cat("Grid job had error: \n")
				for(l in 1:length(res))
					cat(res[l],"\n")
				cat(paste("if file exists, see ", .grid$localDir,jobs[[i]]$name, "-script.Rout, otherwise activate debug mode\n", sep=""))
				del=FALSE
				grid.unlock(.grid$gridJobs[[i]]$var)
				errOcc=TRUE
			}
			options(warn=warn)
		}
	}
      if(!errOcc)  # if no error
	{
		if(is.null(grid.input.Parameters)){
			cat("Grid job had try-error, returned NULL \n")
			grid.unlock(.grid$gridJobs[[i]]$var)
			errOcc=TRUE
		}
	    else if(inherits(grid.input.Parameters, "try-error")){

        	cat("Grid job had try-error, see variable ",jobs[[i]]$var,"for more information\n")
			grid.unlock(.grid$gridJobs[[i]]$var)
			errOcc=TRUE
	   }
	   # return of codetools that something is missing?
	  else 
	  {
		  if(is.null(grid.input.Parameters)){
			  cat("Grid job had try-error, returned NULL \n")
			  grid.unlock(.grid$gridJobs[[i]]$var)
			  errOcc=TRUE
		  }
	  	else if(regexpr(" <anonymous>: no visible binding for global variable ",grid.input.Parameters)[1]>0 || regexpr(" <anonymous>: no visible global function definition for ",grid.input.Parameters)[1]>0)  
  		{
			# test if this and the last codetools message is the same => endless loop
			if(grid.input.Parameters==jobs[[i]]$codeToolsOld)
			{
				cat("cannot load local function/variable:\n", grid.input.Parameters)
				grid.unlock(.grid$gridJobs[[i]]$var)
				.grid$changed=append(.grid$changed, jobs[[i]]$var)
				errOcc=TRUE
			}
			else
				jobs[[i]]$codeToolsOld=grid.input.Parameters
			
			jobs[[i]]$run=jobs[[i]]$run+1
	
			#load values for f , grid.input.Parameters.x, varlist
			load(paste(.grid$localDir,jobs[[i]]$name,"-fx",sep=""))
		
			split=strsplit(grid.input.Parameters,"<anonymous>")
			for(k in 2:length(split[[1]]))
			{
				#find start and end of variable in split[[k]]
				grepRes=gregexpr("no visible binding for global variable", split[[1]][k])[[1]]+40	
				if(grepRes<40)# missed is a function
					grepRes=gregexpr("no visible global function definition for", split[[1]][k])[[1]]+43
				
				grepResEnd=gregexpr("\n", split[[1]][k])[[1]]-2
				
				add=substr(split[[1]][k], grepRes, grepResEnd)
				#	cat(paste(k,"grep var:",add, "\n"))
				if(exists(add))
					jobs[[i]]$varlist = append(jobs[[i]]$varlist, add)
				else{
					if(exists(substr(add, 3, nchar(add)-2)))# convertion problem of not ascii chars
						jobs[[i]]$varlist = append(jobs[[i]]$varlist, substr(add, 3, nchar(add)-2))
					else
					{
						cat("cannot load local function/variable:", add, "\n extracted from line: ",split[[1]][k],"\n")
						grid.unlock(.grid$gridJobs[[i]]$var)
						errOcc=TRUE
					}
				}
			}
			if(!errOcc)
			{
				cat("Variables",jobs[[i]]$varlist,"was missing in Grid call, restarting ...\n")
					
				#save varialbes for reapply
				var=jobs[[i]]$var
				plots=jobs[[i]]$plots
				run=jobs[[i]]$run
				wait=jobs[[i]]$wait
				varl=jobs[[i]]$varlist
				check=jobs[[i]]$check
				batch=jobs[[i]]$batch
				reapply=TRUE
			}
      	}#if no error:
     		 else 
			 if(!errOcc)
			 {
				 .grid$changed=append(.grid$changed, jobs[[i]]$var)
				 if(.grid$verbose){
        			cat("Grid job finished, result written to variable",jobs[[i]]$var, "\n") 
     			 }
			 }
    	}
      	assign(jobs[[i]]$var,grid.input.Parameters,.GlobalEnv)
      }
	  if(!del) 
		  del=TRUE
	  else
		 #delete all files of this job
			if(!.grid$debug)
      	 		unlink(paste(.grid$localDir,jobs[[i]]$name,"-*",sep=""))
			
      jobs <- jobs[setdiff(1:length(jobs),i)]

      if(reapply)
		i=length(jobs)+1
    }
    else{
   	i <- i+1
    }
  }
 # newGrid <- .grid
 # newGrid$gridJobs <- jobs
 # assign(".grid",newGrid, loadNamespace("GridR"))
 .grid$gridJobs <- jobs
 assign(".grid",.grid, loadNamespace("GridR"))
 
#execute job with missing variables:
  if(reapply)
	{
		if(length(grid.input.Parameters.x) > 0){
			param <- paste("grid.input.Parameters.x[[1]]",sep="")
			if(length(grid.input.Parameters.x) > 1){
				for(i in 2:length(grid.input.Parameters.x)){
					param <- paste(param,",grid.input.Parameters.x[[",i,"]]",sep="")
				}
			}
		}
		eval(parse(text=paste("grid.apply(var, grid.input.Parameters.f,",param,", wait=wait, plots=plots, run=run, varlist=varl, check=check, batch=batch)", sep="")))
		reapply=FALSE
	}
	return(TRUE)
}

