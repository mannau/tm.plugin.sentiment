#' Chart Sentiment
#' Chart (stock) sentiment using quantmods plotting capabilities.
#' Some hopefully useful presets have been made.
#' @author Mario Annau
#' @param xts XTS Object Containing at minimum 'Close' and 'Score' fields
#' @param prefix prefix of sentiment field, defaults to "SEMNT"
#' @param sentname name of sentiment score field, defaults to paste(prefix,"polarity", sep = ".")
#' @param volname name of sentiment volume field, defaults to paste(prefix,"volume", sep = ".")
#' @param volcolor Color of Sentiment Score Volume Bars, defaults to "blue"
#' @param na.fill should parts of timeseries be plotted where \code{sentname} or \code{volname} 
#' in \code{xts} is \code{NA}? Could be useful if Sentiment score/volume have been aggregated or
#' data is very sparse. Please note that timeseries then becomes compressed, defaults to TRUE
#' @param omit.na.leading Omits leading NA's in timeseries, defaults to TRUE
#' @param type linetype of Stock Chart, defaults to "line", 
#' @param theme Chart Theme, defaults to "white"
#' @param name Main Title of Sentiment Chart, default is taken from prefix in time series
#' @param TA add Technical analysis Indicators to chart at the beginning as string, defaults to ""
#' @param postTA add Technical analysis Indicators at the end to the chart, defaults to ""
#' @param ... additional parameters to chartSeries function call
#' @seealso Most presets and optional arguments refer to quantmods excellent 
#' \code{\link{chartSeries}} function
#' @export
chartSentiment <- 
function (xts, prefix = "SEMNT", 
		sentname = paste(prefix,"polarity", sep = "."), 
		volname =  paste(prefix,"vol", sep = "."),
		volcolor="blue", 
		na.fill = TRUE,
		omit.na.leading = TRUE,
		type = "line", 
		theme='white',
		name,
		TA = "", postTA = "", ...) 
{
	
	counter = 1
	
	if(omit.na.leading){
		firstnona <- min(which(!is.na(xts[,sentname])))
		xts <- xts[firstnona:NROW(xts),]
	}
	
	if(na.fill){
		xts[is.na(xts)] <- 0
	}
	
	if(missing(name)){
		name = ""
		if(has.Cl(xts))
			name <- strsplit(colnames(Cl(xts)), "\\.", perl = TRUE)[[1]][1]
		else
			name <- strsplit(colnames(xts[,1]), "\\.", perl = TRUE)[[1]][1]
		
		name <- paste("Sentiment", name)
	}

	if(sentname != ""){
		TA <- paste(TA, "addSentiment('", sentname, "');", sep = "")
		counter = counter + 1
	}
		
	if(has.SentVol(xts)){
		TA <- paste(TA, "addSentimentVo('", volname, "');", sep = "")
		counter = counter + 1
	}

	if(has.Vo(xts)){
		TA <- paste(TA, "addVo();", sep = "")
		counter = counter + 1
	}

	if(all(has.Subj(xts), 
			has.Neg_Refs_Per_Ref(xts), 
			has.Pos_Refs_Per_Ref(xts),
			has.Senti_Diffs_Per_Ref(xts))){
		range <- range(na.omit(rbind(Pos_Refs_Per_Ref(xts), 
								Neg_Refs_Per_Ref(xts), 
								Senti_Diffs_Per_Ref(xts), 
								Subj(xts))))
		
		addSubjectivityAll <- newTA(Subj, col=1, yrange = range)
		assign("addSubjectivityAll", addSubjectivityAll, envir = .GlobalEnv)
		
		TA <- paste(TA, "addSubjectivityAll();", sep = "")
		counter = counter + 1 
		
		TA <- paste(TA, "addPos_Refs_Per_Ref(on=", counter, ");", sep = "")
		TA <- paste(TA, "addNeg_Refs_Per_Ref(on=", counter, ");", sep = "")
		TA <- paste(TA, "addSenti_Diffs_Per_Ref(on=", counter, ");", sep = "")
	}else{
		if(has.Subj(xts)){
			TA <- paste(TA, "addSubjectivity();", sep = "")#
		}
		
		if(has.Pos_Refs_Per_Ref(xts)){
			TA <- paste(TA, "addPos_Refs_Per_Ref();", sep = "")
		}
		
		if(has.Neg_Refs_Per_Ref(xts)){
			TA <- paste(TA, "addNeg_Refs_Per_Ref();", sep = "")
		}
		
		if(has.Senti_Diffs_Per_Ref(xts)){
			TA <- paste(TA, "addSenti_Diffs_Per_Ref();", sep = "")
		}
	}

	TA <- paste(TA, postTA, sep = ";")

	
	chartSeries(xts, type = "line", theme = "white",
			name = name,
			TA = TA, ...)
	
#	plotxts <- xts[,sentname]
#	colnames(plotxts) <- paste(prefix, "Close", sep = ".")
#	
#	chartSeries(xts, type = "line", theme = "white",
#			name = name,
#			TA = TA)

}

