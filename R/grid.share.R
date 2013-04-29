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

`grid.share` <-
function(varName){
	if(is.null(.grid$schedulerMode))
	{
		cat("please run grid.init(...) first\n")
		return(FALSE)
	}
	if(!is.character(varName)){
		print("Wrong usage. To share Variable x call grid.share\"x\"\n")
		return(FALSE)
	}
	if(is.null(.grid$nfs$dir))
	{
		print("Error: please run grid.init() first\n")
		return(FALSE)
	}
	if(!.grid$nfs$run)
	{
		print("Error: variableSharing is disabled")
		return(FALSE)
	}
	filePath=paste(.grid$nfs$dir,varName, sep="")
 	#command= eval(parse(text="paste(\"save(file=filePath,\",varName,\")\")"))
	#eval(parse(text=command))
	save(file=filePath, list=varName)
}
