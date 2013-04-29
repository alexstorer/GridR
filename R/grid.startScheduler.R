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


`grid.startScheduler` <-function(verbose=TRUE, intern=FALSE){
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
	if(is.null(.grid$schedulerIp) || is.null(.grid$schedulerPort))
	{
		cat("Error: please run grid.init() first\n")
		return(FALSE)
	}
	if(.grid$schedulerIp=="localhost" || .grid$schedulerIp=="LOCALHOST" || .grid$schedulerIp=="127.0.0.1"){
		system(paste("java de.fhg.iais.kd.djm.ServerStartup",.grid$schedulerPort), wait=FALSE)
		if(!intern){
			.grid$schedulerStarted=TRUE
			assign(".grid",.grid,.GlobalEnv)
		}
		else
			return(TRUE)
	}
	else
	{
		if(verbose)
			cat("The scheduler IP is not localhost, please start it manually on the remote server\n")
		if(intern)
			return(FALSE)
	}
		
}