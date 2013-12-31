#' Preprocess Corpus with functions specified in control.
#' Little helper function to have a little more control of the preprocessing step.
#' Especially the order of the control elements is preserved (as opposed to the
#' preprocessing done in DocumentTermMatrix or TermDocumentMatrix.
#' @author Mario Annau
#' @param corpus \code{\link{Corpus}} object which should be processed
#' @param control list of functions to be applied to corpus
#' @param verbose print preprocessing status information
#' @seealso \code{\link{DocumentTermMatrix}} \code{\link{TermDocumentMatrix}} 
#' @export
preprocessCorpus <-
function(corpus, control, verbose = FALSE){
	
	if(missing(control)){
		control = list( removePunctuation = list(), 
				removeNumbers = list(), 
				tolower = list(), 
				removeWords = list(stopwords("english")),
				stripWhitespace = list(),
				stemDocument = list())
		
	}
	
	if(verbose)
		cat("Starting Preprocessing...\n")
	
	for(n in names(control)){
		if(verbose)
			cat(n, " ...")
		args <- control[[n]]
		if(length(args) == 0){
			corpus <- eval(call("tm_map", corpus, n))
		}else{
			corpus <- eval(call("tm_map", corpus, n, unlist(args)))
		}
		if(verbose)
			cat("Done\n")
	}
	corpus
}

