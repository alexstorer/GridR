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

`grid.batchFunction` <-
function(grid.input.Parameters, fName, yName, varlist, scriptName, remScriptName, errName, condorName, batch, check, noCondor, remoteRPath){
	cmd=grid.getBatchCmd(grid.input.Parameters, batch)
	count=1
	while(count<=length(cmd))
	{
	#submit all jobs	
		#remote script which loads libraries, checks if all functions and variables exist and then executes the main function. result is written to grid.input.Parameters
		# this script is executed by condor
		remscript <- "# automatically generated R script from gridR to be executed serverside
				options(warn=2)\n"
		remscript <- paste(remscript,"err=try(load(\"",fName,"\"))				
						if(inherits(err, \"try-error\")){
							write.table(\"try-error in RemScript: cannot load fName. Is GridR installed on the cluster?\",file=\"",yName, "-",count,"\", quote=FALSE,row.names=FALSE,col.names=FALSE) 
							q()} 
						grid.input.Parameters=c() 
						err=try(library(codetools))
						if(inherits(err, \"try-error\")){
						write.table(\"try-error in RemScript: cannot load library codetools\",file=\"",yName, "-",count,"\", quote=FALSE,row.names=FALSE,col.names=FALSE)\n q() } \n",sep="")
		if(check){
			if(!is.null(varlist))
				for(i in 1:length(varlist)){ 
					remscript=paste(remscript, "if(is.function(",varlist[i],")) checkUsage(",varlist[i],", report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n", sep="")
				}
			remscript=paste(remscript,"checkUsage(grid.input.Parameters.f, report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n if(is.null(grid.input.Parameters)) #otherwise checkUsage reports an error \n",sep="")
		}
		remscript=paste(remscript,"grid.input.Parameters = try(eval(parse(text=\"",cmd[count],"\"))) 
						if(inherits(err, \"try-error\")){
							write.table(paste(\"try-error in RemScript:\",err),file=\"",yName, "-",count,"\", quote=FALSE,row.names=FALSE,col.names=FALSE) \n q()}
						save(list=c(\"grid.input.Parameters\"),file=\"",yName, "-",count,"\")",sep="")
		write.table(remscript,file=paste(remScriptName, "-",count,sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
		
		err=FALSE	
		if(noCondor){
			system(paste(R.home(component="bin"),"/R CMD BATCH --vanilla ", remScriptName, "-",count, sep=""))
		}
		else{
			#make condorscript
			condorScript=paste("Executable     = ",remoteRPath,"
							Universe       = vanilla
							should_transfer_files = YES
							when_to_transfer_output = ON_EXIT
							arguments      = \"CMD BATCH --vanilla --slave ",remScriptName, "-",count,"\"
							Error          = ",errName,"-",count,"
							transfer_input_files =",remScriptName,"-",count,",",fName,"
							transfer_files = ALWAYS
							Queue", sep="")
			write.table(condorScript,paste(condorName, "-",count,sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
			err=try(system(paste("condor_submit ",condorName, "-",count, sep="")))#,intern=TRUE))
		}
		count=count+1
	}		
	count=1
	ret=list()
	#read outputs
	while(count<=length(cmd))
	{
		#wait until result of job "count" is ready 
		while(!file.exists(paste(yName, "-", count, sep=""))){
			#look for condor errors
			if(file.exists(paste(errName, "-", count,sep="")))
			{
				err=scan(file=paste(errName, "-", count,sep=""), what=character(0), quiet=TRUE )
				if(length(err)>0) 
					return(paste("Condor error: ",err, sep=""))
			}
			if(file.exists(paste(scriptName, "out",sep=""))){
				out=scan(file=paste(scriptName, "out",sep=""), what=character(0), sep="\n")
				if(any(grep("^ERROR:", out))) 
					return(paste("RemoteError:", grep("^ERROR:", out, value=TRUE)))
				if(any(grep("^WARNING:", out)))
					return(paste("RemoteError:", grep("^WARNING:", out, value=TRUE)))
			}
			#if no error, wait until result file exists				
			Sys.sleep(5)
		}
		#if result file exists:
		op=options()
		options(show.error.messages=FALSE)
		options(warn=-1)
		err=try(load(paste(yName, "-", count, sep="")))
		options(show.error.messages=TRUE)
		options(op)
		if(inherits(err, "try-error")){
			err2=scan(file=paste(yName, "-", count, sep=""), what=character(0), sep="\n", quiet=TRUE )
			ret[[count]]=c(paste("Remote Error:",err2), paste("result from parameters:",cmd[count]))
		}
		else
			ret[[count]]=grid.input.Parameters#c(y, paste("result from parameters:",cmd[count]))
		count=count+1
	}
	return(ret)
}

