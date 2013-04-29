# transfers a String to a list, which contains vectors of the syntax [[1, 2], [1, 2]] or [1, 2]
`grid.strToList` <-function(input){
	result=list()
	if(is.null(input))
		return(NULL)
	if(length(input)!=1)
		return(input)
	if(nchar(input)==0)
		return(input)
	if(any(substr(input,1,1)!="["))
		return(input)
	if(substr(input,2,2)=="[")
		input=substr(input, 3, nchar(input)-1)
	else
		input=substr(input, 2, nchar(input))
	input= strsplit(input, "\\[")[[1]]
	for(i in 1:length(input)){
		values = strsplit(input[i],"\\]")[[1]][1] #cut the rest behind ]
		values = strsplit(values, ", ")[[1]]
		
		#try to convert to numeric
		warn=getOption("warn")
		options(warn=-1)
		tmp = as.numeric(values)
		options(warn=warn)
		if(any(is.na(tmp)))
			result[[i]]=values
		else
			result[[i]]=tmp
	}
	return(result)
}