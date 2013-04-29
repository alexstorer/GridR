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

`grid.makeWSFiles` <-
function(webservice, fName, plots, scriptName, yName, remScriptName, psName, varlist, cmd, check, remLibFilename=NULL, batch=FALSE) {
	
    # create local R script which connects to the Webservice and uploads the library file
	script="options(warn=2)\n"
	path=paste("\\\"",.grid$javaClientPath, "\\\"", .grid$classSeparator, "\\\"",.grid$javaClientPath,"axis.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"activation.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"jaxrpc.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"commons-logging-1.0.4.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"commons-discovery.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"mail.jar\\\"",.grid$classSeparator, "\\\"",.grid$javaClientPath,"wsdl4j.jar\\\" ",sep="")
	#path=paste(.grid$javaClientPath,.grid$classSeparator,.grid$javaClientPath,"axis.jar",.grid$classSeparator,.grid$javaClientPath,"activation.jar",.grid$classSeparator,.grid$javaClientPath,"jaxrpc.jar",.grid$classSeparator,.grid$javaClientPath,"commons-logging-1.0.4.jar",.grid$classSeparator,.grid$javaClientPath,"commons-discovery.jar",.grid$classSeparator,.grid$javaClientPath,"mail.jar",.grid$classSeparator,.grid$javaClientPath,"wsdl4j.jar ",sep="")
	if((gregexpr("AcgtRWebservice", webservice)[[1]]==1)) {
		url=.grid$acgtUrl		
		grid.makeRemRFile(plots, remScriptName, psName, varlist, cmd, check, remLibFilename=remLibFilename )
		script=paste(script,"ret=try(system(\"java de.fhg.iais.kd.gridr.clients.GridRServiceACGTClient functionExecutionACGT \\\"",.grid$localDir,fName ,"\\\" \\\"",.grid$localDir,remScriptName,"\\\" \\\"",yName,"\\\" \\\"",.grid$acgtUrl, "\\\" ", .grid$myProxyUsername, " ",.grid$pwd, " ",.grid$acgtHost," ",.grid$acgtDn," ", .grid$myProxyHost, " ",.grid$credentialName, " ", .grid$myproxyPort, "\",intern=TRUE))", sep="")
	}
	else {
			print("wrong webservice in makeWSFiles")
			return();
		}
	script=paste(script, "#look for errors
					if(length(ret)==1)
						if(substr(ret,1,7)==\"Error: \"){
							write.table(ret,\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							q(runLast=FALSE)
						}
					if(inherits(ret, \"try-error\")){
						write.table(\"try-error: cannot connect to webservice, check Java Paths and webservice\",\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
						q(runLast=FALSE)
					}		
					out=scan(file=\"",scriptName,"out\", what=character(0), sep=\"\\n\", quiet=TRUE)
					if(any(nchar(out))>0){
						if(any(grep(\"^Exception in thread\", out)) && any(grep(\"Bad version number in .class file\", out))){
							write.table(\"cannot connect to webservice, please install Java 1.6 or newer\",\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							q(runLast=FALSE)
						}
						else
						if(any(grep(\"^Exception in thread\", out))){
							write.table(\"cannot connect to webservice, check Java Paths and webservice\n\",\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							write.table(out,\"",yName,"\",append=TRUE, quote=FALSE,row.names=FALSE,col.names=FALSE )
							q(runLast=FALSE)
						}
						else
						if(any(grep(\"AxisFault\", out))){
							write.table(\"cannot connect to webservice, check webservice\n\",\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							write.table(out,\"",yName,"\",append=TRUE, quote=FALSE,row.names=FALSE,col.names=FALSE )
							q(runLast=FALSE)
						}
						else{	
							write.table(\"Error:\n\",\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							write.table(out,\"",yName,"\",append=TRUE, quote=FALSE,row.names=FALSE,col.names=FALSE )	
							q(runLast=FALSE)
							}
					 }\n", sep="")
	if(gregexpr("AcgtRWebservice", webservice)[[1]]!=1)
		script=paste(script, "while(TRUE){
						res= system(paste(\"",paste("java -classpath ",path,"Client ", yName, " ",webservice, " ",url, " ", sep=""),"\",ret, sep=\"\"), intern=TRUE)			
						if(length(res)!=1) #error
						{
							write.table(res,\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
							break
						}
						if(res==\"yes\") #file is copied
							break
						else
							if(res==\"no\"){#file does not exists remotely
								Sys.sleep(10)
								next
							}
						#else error:
						write.table(res,\"",yName,"\",quote=FALSE,row.names=FALSE,col.names=FALSE )
						break
					}
					q(runLast=FALSE)", sep="")
	
	write.table(script,scriptName,quote=FALSE,row.names=FALSE,col.names=FALSE)
}

