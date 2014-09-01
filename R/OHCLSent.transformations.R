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
#' @rdname trans
#' @export
`Subj` <-
function(x)
{
	if(has.Subj(x))
		return(x[,grep('Subjectivity',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Score"')
}

#' @rdname trans
#' @export
`has.Subj` <-
function(x,which=FALSE)
{
	loc <- grep('Subjectivity',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @rdname trans
#' @export
`Polarity` <-
		function(x)
{
	if(has.Polarity(x))
		return(x[,grep('Polarity',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Polarity"')
}

#' @rdname trans
#' @export
`has.Polarity` <-
function(x,which=FALSE)
{
	loc <- grep('Polarity',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @rdname trans
#' @export
`Pos_Refs_Per_Ref` <-
function(x)
{
	if(has.Pos_Refs_Per_Ref(x))
		return(x[,grep('Pos_Refs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Pos_Refs_Per_Ref"')
}

#' @rdname trans
#' @export
`has.Pos_Refs_Per_Ref` <-
		function(x,which=FALSE)
{
	loc <- grep('Pos_Refs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @rdname trans
#' @export
`Neg_Refs_Per_Ref` <-
		function(x)
{
	if(has.Neg_Refs_Per_Ref(x))
		return(x[,grep('Neg_Refs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Neg_Refs_Per_Ref"')
}

#' @rdname trans
#' @export
`has.Neg_Refs_Per_Ref` <-
		function(x,which=FALSE)
{
	loc <- grep('Neg_Refs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @rdname trans
#' @export
`Senti_Diffs_Per_Ref` <-
		function(x)
{
	if(has.Senti_Diffs_Per_Ref(x))
		return(x[,grep('Senti_Diffs_Per_Ref',colnames(x),ignore.case=TRUE)])
	stop('subscript out of bounds: no column name containing "Senti_Diffs_Per_Ref"')
}

#' @rdname trans
#' @export
`has.Senti_Diffs_Per_Ref` <-
function(x,which=FALSE)
{
	loc <- grep('Senti_Diffs_Per_Ref',colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}

#' @rdname trans
#' @export
`SentVol` <-
function(x, prefix = "SEMNT")
{
	name = paste(prefix, "vol", sep=".")
	
	if(has.SentVol(x, prefix = prefix))
		return(x[,grep(name,colnames(x),ignore.case=TRUE)])
	stop(paste('subscript out of bounds: no column name containing"', name, '"'))
}

#' @rdname trans
#' @export
`has.SentVol` <-
function(x,which=FALSE, prefix = "SEMNT")
{
	name = paste(prefix, "vol", sep=".")
	loc <- grep(name,colnames(x),ignore.case=TRUE)
	if(!identical(loc,integer(0)))
		return(ifelse(which,loc,TRUE))
	ifelse(which,loc,FALSE)
}




