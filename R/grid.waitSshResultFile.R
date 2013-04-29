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

`grid.waitSshResultFile` <-
function(remOutput, RoutFile){
	
	exe=paste("tmp=system(",paste("\"ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \\\"cd ",.grid$ssh$remotePath," && ls\\\" \"",sep=""),", intern=TRUE)
	while(!(\"",remOutput,"\" %in% tmp)) {
		Sys.sleep(5)
		tmp=system(",paste("\"ssh ",.grid$ssh$username,"@",.grid$ssh$ip," \\\"cd ",.grid$ssh$remotePath," && ls\\\" \"",sep=""),", intern=TRUE)
	}	
	system(",paste("\"scp -B ",.grid$ssh$username,"@",.grid$ssh$ip,":",.grid$ssh$remotePath, RoutFile," ",.grid$localDir,"\"",sep=""),", intern=TRUE)
	system(",paste("\"scp -B ",.grid$ssh$username,"@",.grid$ssh$ip,":",.grid$ssh$remotePath, remOutput, " " ,.grid$localDir,"\"",sep=""),", intern=TRUE)",sep="")
	if(!.grid$debug)
		exe=paste(exe,"\nsystem(",paste("\"ssh -f ",.grid$ssh$username,"@",.grid$ssh$ip," \\\"cd ",.grid$ssh$remotePath," && rm ",.grid$uniqueName,"*\\\"\"",sep=""),", intern=TRUE)
		", sep="")
	write.table(exe,file=paste(.grid$localDir,.grid$uniqueName,"-waitForReturn.R",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
}

