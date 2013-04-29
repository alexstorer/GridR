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

`grid.lock` <-
function(varName){
  #if code is changed, change it in grid.init,too
  # test if write-locked
  if(varName %in% .grid$lock$varName){
    cat("Object", varName, "has lock")
  }
  else{
    # do locking
    ex <- exists(varName)
    if(! ex){
      assign(varName,NULL,.GlobalEnv)
    }
    val <- eval(parse(text=varName))
    functionname <- paste(".gridlock.",varName,sep="")
    #f <- function(...){
    #  grid.callback()
    #  stop(paste("Object"+varName+"has write lock from grid function"))
    #}&& length(x)==0
	text <- paste(functionname,"<- function(...){
								grid.callback()
								if(!(length(list(...))==0 && any(get(\".grid\", loadNamespace(\"GridR\"))$changed==varName)))
									stop(paste(\"Object\",varName,\"has write lock from grid function\"))
								else
								{
									eval(parse(text=varName))
								}
							 }")
#	text <- paste(functionname,"<- function(...){grid.callback(); stop(paste(\"Object\",varName,\"has write lock from grid function\"\n)) }")
    eval(parse(text=text))
    rm(list=varName,pos=1)
    makeActiveBinding(varName,eval(parse(text=functionname)),.GlobalEnv)

    i <- length(.grid$lock$varName)+1
    .grid$lock$varName[i] <- varName
    .grid$lock$writeLock[i] <- TRUE
    .grid$lock$value[[i]] <- val
    .grid$lock$exists[i] <- ex
    #assign(".grid",.grid,.GlobalEnv)
		assign(".grid",.grid, loadNamespace("GridR"))
  }
}