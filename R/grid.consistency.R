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

`grid.consistency` <-
function() { # check if there are errors in .grid structure 
if(is.null(.grid$schedulerMode))
{
	cat("please run grid.init(...) first\n")
	return(FALSE)
}
wd=getwd()	
setwd(.grid$localDir)
jobVars=c()
jobNames=c()
 # check if all running jobs are locked
if(length(.grid$gridJobs)>0)
  for(i in 1:length(.grid$gridJobs)){
	jobVars=append(jobVars, .grid$gridJobs[[i]]$var)
        jobNames=append(jobNames, .grid$gridJobs[[i]]$name)

      if(!.grid$gridJobs[[i]]$var %in% .grid$lock$varName)
	{
	cat("Job ", i, " exists, but variable ", .grid$gridJobs[[i]]$var, "is not locked!\n lock it? (y/n)")
	res=scan(file=stdin(), what=character(0), nmax=1)
	if(res=="y" || res=="yes")
	   grid.lock(.grid$gridJobs[[i]]$var)
	}
   }
 # check if all locked jobs exists
if(length(.grid$lock$varName)>0)
   for(i in 1:length(.grid$lock$varName)){
      if(!.grid$lock$varName[i]%in% jobVars)
	{
	cat(.grid$lock$varName[i], "is locked, but Job does not exists!\n unlock? (y/n)\n")
        res=scan(file=stdin(), what=character(0), nmax=1)
	if(res=="y" || res=="yes")
	   grid.unlock(.grid$lock$varName[i])
      }
   }
 # check if there are files without a running job
files=dir()
del=c()
fileJobs=c()
if(length(files)>0)
   for(i in 1:length(files)){
	if(regexpr(paste("^grid-", Sys.info()["nodename"],"[0-9]*-[0-9]*-[0-9]*-[0-9]*-[0-9]*-[0-9]*-[0-9]*-[0-9]*-.*$",sep=""), files[i], perl=TRUE)[1]>0)
	   {# found a jobfile
	   #look if a job to that file exists
	   found=FALSE
	   if(length(jobNames)>0)
	     for(j in 1:length(jobNames))
		if(pmatch(jobNames[j], files[i], nomatch=0)==1)
			found=TRUE
	   if(found==FALSE)
	   {   #no Job found
		del=append(del, files[i])
	   }
	}
	
   }
if(!is.null(del)) 
  {
  print(del)
  cat("no running job for these files found.  Delete all files? (y/n)\n")
  res=scan(file=stdin(), what=character(0), nmax=1)
	if(res=="y" || res=="yes"){
	   for (i in 1:length(del))
		unlink(del[i])
	}
	else {
	     for (i in 1:length(del)) {
		cat("no running Job for File", del[i], "found. Delete this file? (y/n)\n")
		res[i]=scan(file=stdin(), what=character(0), nmax=1)
		if(res[i]=="y" || res[i]=="yes"){
			unlink(del[i])
		}
	     }
	}

  }
setwd(wd)
}

