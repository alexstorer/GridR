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

`grid.check` <-
function(grid.input.Parameters.f,x="",varlist=c(), fName="", intern=FALSE) {
if(is.null(.grid$schedulerMode))
{
	cat("please run grid.init(...) first\n")
	return(FALSE)
}
wd=getwd()
setwd(.grid$localDir)
end=FALSE
fMissing = paste(.grid$uniqueName, "-missing", sep="")
localScript=paste(.grid$uniqueName, "-localScript.R", sep="")
codeToolsOld=""

if(fName=="")
{
   fName <- paste(.grid$uniqueName,"-fxc",sep="") #dont overwrite other fName!!!
   save(list=c("grid.input.Parameters.f",varlist),file=fName)
}
while(!end)
{
    script <- paste("load(\"", fName,"\") \n",sep="")
    script <- paste(script, "grid.input.Parameters=c() \n library(codetools) \n",sep="")
    if(!is.null(varlist))
		for(i in 1:length(varlist))
	      script=paste(script, "if(is.function(",varlist[i],")) checkUsage(",varlist[i],", report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n", sep="")
    script=paste(script,"checkUsage(grid.input.Parameters.f, report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n save(list=c(\"grid.input.Parameters\"),file=\"", fMissing, "\")",sep="")

	write.table(script,file=localScript,quote=FALSE,row.names=FALSE,col.names=FALSE)

	system(paste(R.home(component="bin"), "/R CMD BATCH --vanilla \"",localScript,"\"", sep=""))
	grid.input.Parameters=NULL
	if(file.exists(fMissing))
		load(fMissing)
	else
	{
		cat("Error cannot check for missing variables, check-result file is missing\n if an internal function is used, its important to set check=FALSE")
		end=TRUE
	}
	if(!is.null(grid.input.Parameters))
	{
		# test if this and the last codetools message is the same => endless loop
		if(grid.input.Parameters==codeToolsOld)
		{
			cat("loop: cannot load local function/variable:\n", grid.input.Parameters)
			end=TRUE
			if(!intern)
				return(FALSE)
		}
		else
		{
		codeToolsOld=grid.input.Parameters
	#	grepRes=gregexpr("‘", y)[[1]]
	#	grepResEnd=gregexpr("’", y)[[1]]
	#	for(k in 1:length(grepRes))
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
					varlist = append(varlist, add)
				else
				{
					cat("cannot load local function/variable:", add, "\n extracted from line: ",split[[1]][k],"\n")
					end=TRUE
					if(!intern)
						return(FALSE)
				}	
			}
	
		save(list=c("x","grid.input.Parameters.f",varlist),file=fName)
		}
	}
	else
	{
		end=TRUE
	}
}
if(!.grid$debug){
	unlink(localScript)
	unlink(paste(localScript,"out", sep=""))
	unlink(fMissing)
}
setwd(wd)
if(intern)
	return(varlist)
else {
	cat("needed variables are:\n")
	return(varlist)
}
}

