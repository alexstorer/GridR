# transfers a list which could contain another list to a string ie: [1 2][3 4][hallo 23123][1 {a b}]
`grid.listToStr` <-function(input){
	inputStr=""
	for(i in 1:length(input))
	{
		inputStr=paste(inputStr,"[", sep="")
		#each parameter
		for(j in 1:length(input[[i]])){
			if(length(input[[i]][[j]])==1)
				inputStr = paste(inputStr, input[[i]][j], " ", sep="")
			else{
				#entry is another array
				inputStr=paste(inputStr,"{", sep="")
				for(k in 1:length(input[[i]][[j]])){
					inputStr = paste(inputStr, input[[i]][[j]][k], " ", sep="")	
				}
				#cut last space 
				inputStr = substr(inputStr, 1, nchar(inputStr)-1)
				inputStr=paste(inputStr,"} ", sep="")
			}
		}
		#cut last space 
		inputStr = substr(inputStr, 1, nchar(inputStr)-1)
		inputStr=paste(inputStr,"]", sep="")
	}
	return(inputStr)
}