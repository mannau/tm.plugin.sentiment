#' Get Relevant Part from Text Document.
#' Search for the first and last occurence of items in Text Document,
#' get sentence boundaries and extract text. 
#' Function can be used in tm_map wrapper
#' @author Mario Annau
#' @param td TextDocument
#' @param items character vector of items to be searched, eg. c("Microsoft", "MSFT")
#' @param boundaries defined sentence boundaries in perl-regex syntax
#' @param matches.only Return number of matches only
#' @param matches only should only matches be annotated, defaults to FALSE
#' @param fieldname name which should be used for meta field in Text Document to be annotated
#' @seealso \code{\link{tm_map}}
#' @export
getRelevant <-
function(td, items, boundaries, matches.only = FALSE, fieldname = "matches"){
	if(missing(boundaries)){
		boundaries = "\\."
	}
	itempatt <- paste("\\W(",paste("(", items, ")", collapse = "|", sep = ""),")\\W", sep = "")
	matches <- gregexpr(itempatt, Content(td), ignore.case = TRUE, perl = TRUE,  fixed = FALSE, useBytes = FALSE)
	if(!matches.only){
		startend <- range(matches)
		delimmatches <- gregexpr(boundaries, Content(td), ignore.case = TRUE, perl = TRUE,  fixed = FALSE, useBytes = FALSE)
		delimmatches <- as.integer(delimmatches[[1]])
		textlen <- nchar(Content(td))
		sentstart <- max(c(delimmatches[delimmatches < startend[1]], 0))
		sentend <- min(c(delimmatches[delimmatches > startend[2]], textlen))
		subtext <- substr(Content(td), sentstart+1, sentend)
		Content(td) <- substr(Content(td), sentstart, sentend)
	}
	if(any(matches[[1]] == -1)){
		meta(td, fieldname) <- 0
	}
	else{
		meta(td, fieldname) <- length(matches[[1]])
	}
	getRelevant <- td
}

