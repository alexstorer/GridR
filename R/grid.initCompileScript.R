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

`grid.initCompileScript` <-function(){
	#if(.grid$historyCallback!=NULL)
	#{
	#	cat("cannot start compileScript, because it is started\n")
	#	return
	#}
	grid.historyTaskCallback <- function(...){
		.grid$hisrory = paste(.grid$history, list(...)[[1]], "\n", sep="")
		assign(".grid", .grid, loadNamespace("GridR"))
		return(TRUE)
	}
	.grid$historyCallback = addTaskCallback(grid.historyTaskCallback)
	assign(".grid", .grid, loadNamespace("GridR"))
	
}