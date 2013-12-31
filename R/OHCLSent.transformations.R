#' Simple XTS transformations for (OHLC) Stock Sentiment XTS Objects
#' @aliases
#' Subj has.Subj
#' Polarity has.Polarity 
#' Pos_Refs_Per_Ref has.Pos_Refs_Per_Ref 
#' Neg_Refs_Per_Ref has.Neg_Refs_Per_Ref
#' Senti_Diffs_Per_Ref has.Senti_Diffs_Per_Ref
#' SentVol has.SentVol
#' @param x xts object 
#' @seealso \code{quantmod}
#' @export 
#' Subj has.Subj 
#' Polarity has.Polarity 
#' Pos_Refs_Per_Ref has.Pos_Refs_Per_Ref 
#' Neg_Refs_Per_Ref has.Neg_Refs_Per_Ref
#' Senti_Diffs_Per_Ref has.Senti_Diffs_Per_Ref
#' SentVol has.SentVol
`Subj` <-
function(x)
{
	if(has.Subj(x))
		return(x[,grep('Subjectivity',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Score"')
}

#' @nord
`has.Subj` <-
function(x,which=FALSE)
{
	loc <- grep('Subjectivity',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @nord
`Polarity` <-
		function(x)
{
	if(has.Polarity(x))
		return(x[,grep('Polarity',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Polarity"')
}

#' @nord
`has.Polarity` <-
function(x,which=FALSE)
{
	loc <- grep('Polarity',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @nord
`Pos_Refs_Per_Ref` <-
function(x)
{
	if(has.Pos_Refs_Per_Ref(x))
		return(x[,grep('Pos_Refs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Pos_Refs_Per_Ref"')
}

#' @nord
`has.Pos_Refs_Per_Ref` <-
		function(x,which=FALSE)
{
	loc <- grep('Pos_Refs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @nord
`Neg_Refs_Per_Ref` <-
		function(x)
{
	if(has.Neg_Refs_Per_Ref(x))
		return(x[,grep('Neg_Refs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Neg_Refs_Per_Ref"')
}

#' @nord
`has.Neg_Refs_Per_Ref` <-
		function(x,which=FALSE)
{
	loc <- grep('Neg_Refs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @nord
`Senti_Diffs_Per_Ref` <-
		function(x)
{
	if(has.Senti_Diffs_Per_Ref(x))
		return(x[,grep('Senti_Diffs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Senti_Diffs_Per_Ref"')
}

#' @nord
`has.Senti_Diffs_Per_Ref` <-
function(x,which=FALSE)
{
	loc <- grep('Senti_Diffs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @nord
`SentVol` <-
function(x, prefix = "SEMNT")
{
	name = paste(prefix, "vol", sep=".")
	
	if(has.SentVol(x, prefix = prefix))
		return(x[,grep(name,colnames(x),ignore.case=TRUE)])
	stop(paste('subscript out of bounds: no column name containing"', name, '"'))
}

#' @nord
`has.SentVol` <-
function(x,which=FALSE, prefix = "SEMNT")
{
	name = paste(prefix, "vol", sep=".")
	loc <- grep(name,colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}




