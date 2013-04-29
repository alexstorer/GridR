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

`grid.compileScript` <-function(path){
	if(is.null(.grid$schedulerMode))
	{
		cat("please run grid.init(...) first\n")
		return(FALSE)
	}
	
	savehistory(path)
	#delete last line
	res = scan(file=path, what=character(0), sep="\n", quiet=TRUE )
#	res=.gridhistory
#	cat(.gridhistory)
	copy=vector()
	copyTmp=vector()
	
	write.table(0, file=paste(path,".out",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	for(i in 1:length(res)-1)
	{
		copy[i]=res[i]
		copyTmp[i]=paste("parse(text=\"", gsub("\\\"","\\\\\"",res[i]),  "\")\n write.table(",i,", file=\"",path,".out\",quote=FALSE,row.names=FALSE,col.names=FALSE)", sep="")
	}
	begin = length(res)
	write.table(copy,file=path,quote=FALSE,row.names=FALSE,col.names=FALSE)
	tmpPath = paste(path, ".tmp", sep="")
	write.table(copyTmp,file=tmpPath,quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	a=system(paste(R.home(component="bin"),"/R CMD BATCH ", tmpPath,sep=""), intern=TRUE)
	errorInLine = scan(file=paste(path, ".out",sep=""), what=character(0), sep="\n", quiet=TRUE )
#	if(errorInLine[1]=="-1")
#	{
#		cat("Script is written, but there is an Error in it\n", errorInLine[2])
#		return
#	}
	if( as.numeric(errorInLine[1]) != length(res)-1)
	{
		cat("Script is written, but there is an Error in Line ", (as.numeric(errorInLine[1])+1), "\n")
	}
	unlink(tmpPath)
	unlink(paste(tmpPath, ".Rout", sep=""))
	unlink(paste(path,".out",sep=""))
}