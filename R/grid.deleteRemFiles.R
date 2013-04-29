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



###TODO not needed

`grid.deleteRemFiles` <-
		function(file){
	#file is an vector of ip:filepath
	split = strsplit(file, ":")
	cat("input:" , file, "\n")
	#ip=split[[i]][1]
	#file=split[[i]][2]ie "grid/grid-linux-9nop-6033-2008-10-20-16-11-48-0-y.dat"
	deleted=vector()
#	cat("deleteRemFiles", length(split), "\n")
	for(i in 1:length(split))
	{
		#each entry
		#delete end
		greped=gregexpr("-",split[[i]][2])[[1]]
		pos=greped[length(greped)]# #last "-"
		beginning=substr(split[[i]][2],1,pos)
#cat("beginning: ",beginning, "\n")
		if(is.na(match(beginning, deleted))) # if not yet deleted
		{
			#delete beginning*
			if(.grid$system=="linux"){
				cat("delete remote\n")
				err=system(paste("ssh ",.grid$ssh$username,"@", split[[i]][1]," \"rm ",beginning,"*\"", sep=""),intern=TRUE)
				if(length(err)!=0) {
					print(paste("Error, cannot copy files from remote host\n", err, sep=""))
					return()
				}	
			}
			else
			{
				cat("delete remote\n")
				system(paste("java de.fhg.iais.kd.gridr.interfaces.SshDeleteRemFile ", .grid$ssh$ip, " ",.grid$ssh$username, " \"",.grid$ssh$key, "\" \"", .grid$localDir ,"\" \"" , beginning, "*\"",sep=""), wait=FALSE)
			}
		
		}
		else
			deleted[length(deleted)+1]=beginning
		
	}
	
}