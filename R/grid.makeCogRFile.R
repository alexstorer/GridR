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

`grid.makeCogRFile` <-
function(scriptName, remScriptName, fName, yName){
	script <- paste("options(warn=2)
					#check if proxy certificate is valid
					err=system(\"grid-proxy-info\", intern=TRUE)
					if(any(gregexpr(\"timeleft : 0 sec\", err)!=-1) || any(gregexpr(\"timeleft : 0:00:00\", err)!=-1)){
					write.table(paste(c(\"cannot get a valid credential: \\n\")),\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE)
					return(FALSE)}
					#copy remote R script file to execution host 
					err=system(\"",paste("cog-file-transfer -s file://",.grid$localDir,remScriptName," -d gsiftp://",.grid$globusHost,":2811",.grid$cogDir,remScriptName, sep=""),"\", intern=TRUE)
					if(length(err)>0)
					write.table(paste(c(\"cannot copy file 1 to remote machine:\\n\",err)),\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE)
					#copy lib file
					err=system(\"",paste("cog-file-transfer -s file://",.grid$localDir,fName," -d gsiftp://",.grid$globusHost,":2811",.grid$cogDir,fName, sep=""),"\", intern=TRUE)
					if(length(err)>0)
					write.table(paste(c(\"cannot copy file 2 to remote machine:\\n\",err)),\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE)
					#execute job remotely
					err=print(system(\"",paste("cogrun -s ",.grid$globusHost," -d ",.grid$cogDir," -e /usr/bin/R -args \\\"CMD BATCH --vanilla ",remScriptName,"\\\"",sep=""),"\", intern=TRUE))
					if(err!=(\"Job completed\"))
					write.table(paste(\"cannot execute job on remote machine:\\n\",paste(err, collapse=\"\\n\")),\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE)
					#copy result back
					err=system(\"",paste("cog-file-transfer -s gsiftp://",.grid$globusHost,":2811",.grid$cogDir,yName, " -d file://",.grid$localDir,yName, sep=""),"\", intern=TRUE)
					if(length(err)>0)
					write.table(paste(c(\"cannot copy result file back from remote machine:\\n\",err)),\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE)",sep="")
	write.table(script,scriptName,quote=FALSE,row.names=FALSE,col.names=FALSE)
}

