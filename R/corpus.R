#' @nord
"getField" <- function(x, ...) UseMethod("getField", x)
#' Get Meta Data Field in VCorpus as Vector
#' Get Meta Data Field in VCorpus as Vector
#' @param x VCorpus object
#' @param fieldname character which specifies meta data field name in corpus
#' @author Mario Annau
#' @S3method getField VCorpus
"getField.VCorpus" <- 
function(x, fieldname){
	field <- do.call("c",lapply(x, function(y) meta(y, fieldname)))
	names(field) <- NULL
	field
}

#' @nord
"setField" <- function(x, ...) UseMethod("setField", x)
#' Set Meta Data Field in VCorpus from Vector
#' Set Meta Data Field in VCorpus from Vector
#' @param x VCorpus object
#' @param fieldname character which specifies meta data field name in corpus
#' @param value vector which values should be used
#' @author Mario Annau
#' @note vector must be of same length as Corpus
#' @S3method setField VCorpus
"setField.VCorpus" <- 
function(x, fieldname, value){
	for(i in 1:length(x)){
		meta(x[[i]], fieldname) <- value[i]
	}
	x
}
