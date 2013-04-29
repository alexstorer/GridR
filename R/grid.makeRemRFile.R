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

`grid.makeRemRFile` <-
function(plots, remScriptName, psName, varlist, cmd, check, outputFile="y.dat", remLibFilename="rCodeLib"){
	# create script which loads libraries, checks variables and executes the function
	remscript <- "# automatically generated R script from gridR to be executed serverside\n
					options(warn=2)\n"
	if(plots){
		remscript <- append(remscript, paste("err=try(library(graphics)) ; if(inherits(err, \"try-error\")){
						write.table(\"try-error in RemScript: cannot load library graphics\",file=\"",outputFile,"\", quote=FALSE,row.names=FALSE,col.names=FALSE);q() };",
				       "postscript(file=\"",psName,"\")",sep=""))
	}
	remscript <- paste(remscript,"err=try(load(\"",remLibFilename,"\")); if(inherits(err, \"try-error\")){
					write.table(\"try-error in RemScript: cannot load transferred library file. Is GridR installed on remote side? \",file=\"",outputFile,"\", quote=FALSE,row.names=FALSE,col.names=FALSE) ;q()}
					grid.input.Parameters=c()
					err=try(library(codetools)); if(inherits(err, \"try-error\")){
					write.table(\"try-error in RemScript: cannot load codetools library\",file=\"",outputFile,"\", quote=FALSE,row.names=FALSE,col.names=FALSE) ;q()}
					\n",sep="")
	if(check){
		if(!is.null(varlist))
			for(i in 1:length(varlist)){ 
				remscript=paste(remscript, "if(is.function(",varlist[i],")) checkUsage(",varlist[i],", report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n", sep="")}
		remscript=paste(remscript,"checkUsage(grid.input.Parameters.f, report=function(s){assign(\"grid.input.Parameters\",paste(grid.input.Parameters, s), .GlobalEnv)}) \n if(is.null(grid.input.Parameters)) #otherwise checkUsage reports an error \n", sep="")
	}
	remscript=paste(remscript,"grid.input.Parameters = try(eval(parse(text=\"",cmd,"\"))) \n save(list=c(\"grid.input.Parameters\"),file=\"",outputFile,"\")",sep="")
	if(plots){
		remscript <- append(remscript,"dev.off()")
	}
	write.table(remscript,file=paste(remScriptName,sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
}

