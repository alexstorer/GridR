# transfers a String to a vector of the syntax [1, 2, 3]
`grid.strToVector` <-function(input){
	if(is.null(input))
		return(NULL)
	if(length(input)!=1)
		return(input)
	if(nchar(input)==0)
		return(input)
	if(any(substr(input,1,1)!="["))
		return(input)
	input=substr(input, 2, nchar(input)-1) # 1, 2, 3
	result= strsplit(input, ", ")[[1]]
	#try to convert to numeric
	warn=getOption("warn")
	options(warn=-1)
	tmp = as.numeric(result)
	options(warn=warn)
	if(!any(is.na(tmp)))
		result=tmp
	return(result)
}
