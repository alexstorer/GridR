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

`grid.shareCallback` <-
function(...){

	#load all vars in nfs path if the are newer than the loaded version
	files=list.files(.grid$nfs$dir)
	loaded = names(.grid$nfs)
	
	for(file in files){
		
		#eval(parse(text=paste(".grid$nfs$", file, sep="")))
		# if file is not loaded before									or a newer file exists
		path=paste(.grid$nfs$dir, file, sep="")
		if(!(file %in% loaded) ||  (file %in% loaded && eval(parse(text=paste(".grid$nfs$",file,"!= file.info(path)[,\"mtime\"]",sep="")))))
		{
			warn=getOption("warn")
			options(warn=-1)
			#load that file
			errLoad=try(load(path, envir=.GlobalEnv), silent=TRUE)
			options(warn=warn)
			if(!inherits(errLoad, "try-error")) {
				#and save the new modified time if there is no error
				eval(parse(text=paste(".grid$nfs$",file,"=file.info(path)[,\"mtime\"]",sep="")))
				print(paste("new version loaded of ",file, sep=""))
			}
		}
		#t1=file.info("/home/mlohmeyer/nfs/x")[,"mtime"]

	}
	
	assign(".grid",.grid, env=loadNamespace("GridR"))
	return(TRUE)
}