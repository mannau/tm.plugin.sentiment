#' Add Sentiment Field to chobs
#' Adds one of the following sentiment fields to the current chob:
#' Subjectivity, Polarity, Pos_Refs_Per_Ref, Pos_Refs_Per_Ref, Neg_Refs_Per_Ref, 
#' Senti_Diffs_Per_Ref, SentVol
#' @aliases addSubjectivity addPolarity addPos_Refs_Per_Ref addNeg_Refs_Per_Ref
#' addSenti_Diffs_Per_Ref addSentVol
#' @author Mario Annau
#' @include OHCLSent.transformations.R
#' @export addSubjectivity addPolarity addPos_Refs_Per_Ref addNeg_Refs_Per_Ref addSenti_Diffs_Per_Ref addSentVol
addSubjectivity <- newTA(Subj, col=1)
addPolarity <- newTA(Polarity, col=4)
addPos_Refs_Per_Ref <- newTA(Pos_Refs_Per_Ref, col=3)
addNeg_Refs_Per_Ref <- newTA(Neg_Refs_Per_Ref, col=2)
addSenti_Diffs_Per_Ref <- newTA(Senti_Diffs_Per_Ref, col=5)
addSentVol <- newTA(SentVol, col=6)

#' Add Sentiment Scores as barchart to current chob
#' @author Mario Annau
#' @param fieldname Specifies fieldname in XTS object which contains sentiment scores
#' @param on Screen on which to plot
#' @seealso \code{\link{addSentimentVo}} \code{\link{chartSentiment}}
#' @references adapted from Jeffrey A. Ryan's \code{\link{quantmod}} package  
#' @export
# TODO remove sentimentFUN parameter 
`addSentiment` <- 
function(fieldname, on = NA){
	lchob <- quantmod:::get.current.chob()
	x <- as.matrix(lchob@xdata)
#	if (!lchob@show.vol || !has.Vo(x)) 
#		return(invisible(new("chobTA", new = FALSE, name = "chartNULL", 
#								call = match.call())))
	Volumes = NULL
	if(missing(fieldname)){
		Volumes <- Polarity(x)
		fieldname = "Polarity"
	}else{
		Volumes <- x[,fieldname]
	}
	
	#Volumes <- x[,fieldname]
	max.vol <- max(Volumes, na.rm = TRUE)
	
	bar.col <- ifelse(Volumes > 0, lchob@colors$up.col, lchob@colors$dn.col)
	
	border.col <- ifelse(is.null(lchob@colors$border), bar.col, 
			lchob@colors$border)
	bar.col <- bar.col[lchob@xsubset]
	chobTA <- new("chobTA")
	
	if (any(is.na(on))) {
		chobTA@new <- TRUE
	}
	else {
		chobTA@new <- FALSE
		chobTA@on <- on
	}

	chobTA@TA.values <- (Volumes)[lchob@xsubset]
	chobTA@name <- "chartSent"
	chobTA@call <- match.call()
	chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
			color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
			spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
			x.labels = lchob@x.labels, log.scale = FALSE, 
			bar.col = bar.col, border.col = border.col, time.scale = lchob@time.scale, vol.scale=list(1, fieldname))
	chobTA@params$thin <- ifelse(lchob@type %in% c("bars", "matchsticks"), 
			TRUE, FALSE)
	if (is.null(sys.call(-1))) {
		TA <- lchob@passed.args$TA
		lchob@passed.args$TA <- c(TA, chobTA)
		lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
				0)
		FUN <- quantmod:::chartSeries.chob
		do.call("FUN", list(lchob))
		invisible(chobTA)
	}
	else {
		return(chobTA)
	}

}

#' Add Sentiment Volume as barchart to current chob
#' @author Mario Annau
#' @param fieldname Specifies fieldname in XTS object which contains sentiment volume, default "Volume"
#' @param color Color of barchart, default "blue"
#' @seealso \code{\link{addSentimentVo}} \code{\link{chartSentiment}}
#' @export
addSentimentVo <- function(fieldname, color = "blue"){
	lchob <- quantmod:::get.current.chob()
	x <- as.matrix(lchob@xdata)
#	if (!lchob@show.vol || !has.Vo(x)) 
#		return(invisible(new("chobTA", new = FALSE, name = "chartNULL", call = match.call())))
#	
	#vol.scale <- x@params$vol.scale
	
	Volumes = NULL
	if(missing(fieldname)){
		Volumes <- SentVol(x)
	}else{
		Volumes <- x[,fieldname]
	}
	
	max.vol <- max(Volumes, na.rm = TRUE)
	
	
	bar.col <- ifelse(Volumes > 0, color, "red")
	
	border.col <- ifelse(is.null(lchob@colors$border), bar.col, 
			lchob@colors$border)
	#bar.col <- bar.col[lchob@xsubset]
	bar.col <- color
	
	chobTA <- new("chobTA")
	chobTA@new <- TRUE
	chobTA@TA.values <- (Volumes)[lchob@xsubset]
	chobTA@name <- "chartVo"
	chobTA@call <- match.call()
	chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
			color.vol = TRUE, multi.col = lchob@multi.col, 
			spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
			x.labels = lchob@x.labels, log.scale = FALSE, 
			bar.col = bar.col, border.col = border.col, time.scale = lchob@time.scale, vol.scale=list(1, "Text Documents"))
	chobTA@params$thin <- ifelse(lchob@type %in% c("bars", "matchsticks"), 
			TRUE, FALSE)
	if (is.null(sys.call(-1))) {
		TA <- lchob@passed.args$TA
		lchob@passed.args$TA <- c(TA, chobTA)
		lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
				0)
		FUN <- quantmod:::chartSeries.chob
		do.call("FUN", list(lchob))
#		legend("topleft",
#				legend=c(paste("Test (",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
#				text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.inter=0.95)

		invisible(chobTA)
	}
	else {
		return(chobTA)
	}
	
}

#' Chart Sentiment is a Helper function for \code{\link{addSentiment}}
#' @author Mario Annau
#' @param x chob
#' @seealso \code{\link{addSentiment}}
#' @export 
`chartSent` <-
function(x) {
	# if volume is to be plotted, do so here
	# scale volume - vol.divisor
	if(class(x) != "chobTA") stop("chartSentiment requires a suitable chobTA object")
	Volumes <- x@TA.values
	
	spacing <- x@params$spacing
	width <- x@params$width
	
	x.range <- x@params$xrange
	x.range <- seq(x.range[1],x.range[2]*spacing)
	
#    multi.col <- x@params$multi.col
	color.vol <- x@params$color.vol
	log.scale <- ifelse(x@params$log.scale,"y","")
	
	vol.scale <- x@params$vol.scale
	
	if(x@new) {
		plot.new()
		plot.window(xlim=c(1, x@params$xrange[2] * spacing),
				ylim=c(min(Volumes,na.rm=TRUE),max(Volumes,na.rm=TRUE)),
				log=log.scale)
		coords <- par('usr')
		rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
		abline(h=axTicks(2), col=x@params$colors$grid.col, lty='dotted')
	}
	
	x.pos <- 1 + spacing * (1:length(Volumes) - 1)
	
#	bar.col <- if(x@params$color.vol) {
#				x@params$bar.col
#			} else x@params$border.col
	
	bar.col <- x@params$bar.col
	border.col <- x@params$border.col
	
	if(x@params$thin) {
		# plot thin volume bars if appropriate
		segments(x.pos,0,x.pos,Volumes,col=bar.col)
	} else {
		rect(x.pos-spacing/3,0,x.pos+spacing/3,Volumes,
				col=bar.col,border=border.col)
	}
	legend.text <- list(list(
					legend=c(paste("Sentiment Score(",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
					text.col=c(x@params$colors$fg.col, last(bar.col))
			))
	legend("topleft",
			legend=c(paste("Sentiment Score(",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
			text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.inter=0.95)
	
	
#   text(0, max(Volumes,na.rm=TRUE) * .9, "Volume:",pos=4)
	
#   text(0, max(Volumes,na.rm=TRUE) * .9,
#        paste("\n\n\n",format(last(Volumes)*vol.scale[[1]],big.mark=','), sep = ""), 
#        pos = 4,col=last(bar.col))
	
	axis(2)
	box(col=x@params$colors$fg.col)
	invisible(vector('list',2))
} # }}}


