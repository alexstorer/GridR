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

`grid.getBatchCmd` <-
function(grid.input.Parameters, batch){
	cmd=c()
	batch=sort(batch)
	exe="k=1;"
	for(i in 1:length(grid.input.Parameters)){
		if(i %in% batch){
			#x_i should be sweeped
			exe=paste(exe,paste("for(grid.input.Parameters", i, " in 1:length(grid.input.Parameters[[",i,"]])){;",sep=""), sep="")
		}
		else{
			#x_i is a constant
			exe=paste(exe,paste("grid.input.Parameters", i,"=1;", sep=""), sep="")	
		}
	}
	exe=paste(exe,paste("cmd[k]=paste(\"grid.input.Parameters.f(", sep=""), sep="")
	
	for(p in 1:length(grid.input.Parameters)){
				exe=paste(exe,paste("grid.input.Parameters.x[[",p,"]][[\",grid.input.Parameters", p,",\"]],", sep=""))
	}
	# delete last ,
	exe=substring(exe,1,nchar(exe)-1)
	
	
	exe=paste(exe,")\",sep=\"\");", sep="")	
	exe=paste(exe,"k=k+1;", sep="")
	
	#add the missing }
	for(l in 1:length(batch))
		exe=paste(exe,paste("};"))
	cat(exe, "\n")
	eval(parse(text=exe))
	return(cmd)
}

