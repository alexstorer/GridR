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


`grid.restartJob` <-function(job){
	if(is.null(.grid$schedulerMode))
	{
		cat("please run grid.init(...) first\n")
		return(FALSE)
	}
	if(!.grid$schedulerMode)
	{
		cat("not in scheduler mode, aborting...\n")
		return(FALSE)
	}
	if(!is.integer(job))
	{
		if(!grid.isLocked(job))
		{
			cat("variable ", job , " is not locked, thus no job is running on it\n")
			return(FALSE)
		}
		if(length(.grid$gridJobs)==0)
		{
			cat("there is no job running\n")
			return(FALSE)
		}
		#find Job for variable job
		for ( i in 1:length(.grid$gridJobs))
			if(job== .grid$gridJobs[[i]]$var)
			{
				job=i
				break;
			}
	}
	#now job is the number of the job to abort
	if(job > length(.grid$gridJobs)){
		cat("job" , job , "does not exists\n")
		return
	}
	id=.grid$gridJobs[[job]]$id
	if(id==-1)
	{
		cat("job" , job , "is a local job, aborting\n")
		return(FALSE)
	}
	#create connection to scheduler
	conn = socketConnection(host=.grid$schedulerIp, port=.grid$schedulerPort, blocking=TRUE)
	command=paste("<job>\n<mode>restartJob</mode>\n<username>",.grid$ssh$username,"</username>\n<id>",id,"</id>\n</job>",sep="")
	writeLines(command, conn)
}