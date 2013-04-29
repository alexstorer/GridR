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

`grid.copyFile` <-function(fileIn){
	if(is.null(.grid$schedulerMode))
	{
		cat("please run grid.init(...) first\n")
		return(FALSE)
	}
	
	split = strsplit(fileIn, ":")[[1]]
	ip=split[1]
	file=split[2]
	size=split[3]
	jobId=split[4]
	splitted = strsplit(file, "/")
	#cat("file:", file, "\n") #ie "grid/grid-linux-9nop-6033-2008-10-20-16-11-48-0-y.dat"
	relFilename =splitted[[1]][length(splitted)+1]#filename without path
	#cat(paste("scp -B ",.grid$ssh$username,"@", ip,":",file," ", relFilename," 2>&1", sep=""))
	if(.grid$system=="linux"){												
		err=system(paste("scp -B ",.grid$ssh$username,"@", ip,":",file," ",.grid$localDir, relFilename," 2>&1", sep=""), wait=FALSE ,intern=TRUE)
		if(length(err)!=0) {
			print(paste("Error, cannot copy files from remote host ",ip,"\n", err, sep=""))
			return()
		}	
	}
	else
		system(paste("java de.fhg.iais.kd.gridr.interfaces.SshDownload ", ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , "", "\" ",.grid$debug, " \"",file, "\"",sep=""), wait=FALSE)
	res=vector()
	res[1]=relFilename
	res[2]=size
	res[3]=jobId
	return(res)
}