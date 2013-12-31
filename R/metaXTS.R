#' Calculate xts from scored corpus
#' 
#' @param corpus Scored corpus from which \code{xts} object should be generated, see \code{\link{score}}
#' @param fieldnames Fieldnames to be used from \code{DMetaData(corpus)}, defaults to DMetaData(corpus)
#' @param period Period unit of xts time series object to be returned, defaults to "days", see \code{endpoints}
#' @param k, Period length of xts time series object to be returned, defaults to 1, see \code{endpoints}
#' @param  aggFUN Aggregation function to be used for meta fields, defaults to mean
#' @param symbol \code{xts} object to be merged with sentiment time series, preferably \code{OHLC} time series
#' @param prefix Character string specifying prefix of resulting sentiment time series column names
#' @param join Join which should be performed if symbol is specified
#' @param na.omit remove NA's from resulting time series, see \code{\link{na.omit}}
#' @author Mario Annau
#' @seealso \code{\link{score}} \code{endpoints} (in package \code{xts})
#' @export
#' @importFrom slam col_sums
metaXTS <- function(corpus, 
	fieldnames, 
	period = "days", 
	k=1, 
	aggFUN = mean, 
	symbol, 
	prefix = "SEMNT", 
	join = "inner",
	na.omit = TRUE){

	if(missing(fieldnames)){
		fieldnames = colnames(DMetaData(corpus))[!colnames(DMetaData(corpus)) == "MetaID"]
	}

	df <- prescindMeta(corpus, "DateTimeStamp")
	df <- df[,c("DateTimeStamp", fieldnames)]
	
	if(na.omit){
		df <- na.omit(df)
	}

	xts <- xts(df[,-1], order.by = do.call("c",df[,1]))
	volxts <- xts(rep(1,NROW(xts)), order.by = index(xts))
	
	ep <- endpoints(xts, on = period, k = k)
	
	xts_agg = NULL
	if(length(ep) > 2){
		ldata <- lapply(1:NCOL(xts), function(x) period.apply(xts[,x], ep, aggFUN))
		ldata_all <- do.call("cbind", ldata)
		xts_agg <- xts(ldata_all, order.by = index(xts)[ep])
		volxts_agg <- period.apply(volxts, ep, sum)
	}else{
		#ep <- ep[ep > 0]

		xts_agg <- xts(t(as.data.frame(apply(xts, 2, aggFUN))), order.by = index(xts)[ep]) 
		volxts_agg <- xts(sum(volxts), order.by = index(xts)[ep]) 
	}
	
		   
	colnames(xts_agg) <- colnames(xts)
	colnames(volxts_agg) <- "vol"
	
	scorevolxts_agg <- cbind(xts_agg, volxts_agg)
	colnames(scorevolxts_agg) <- paste(prefix,colnames(scorevolxts_agg), sep=".")
	if(period == "days")
		index(scorevolxts_agg) <- as.Date(index(scorevolxts_agg))
	
	if(!missing(symbol)){
		names <- colnames(symbol)
		symbol <- to.period(symbol, period = period, k = k)
		colnames(symbol) <- names
		scorevolxts_agg <- merge(symbol, scorevolxts_agg, join = join)
	}
	
	return(scorevolxts_agg)
}

