#' Score Corpus
#' Annotate corpus with scores using various functions
#' Implementation generates a \code{TermDocumentMatrix} as a first step and applies specified functions to it in order to generate score.
#' @param corpus Text corpus to be annotated
#' @param control Paramter to control \code{TermDocumentMatrix} generation, if missing default to
#' \code{control = list( 
#'				tolower = TRUE, 
#'				removePunctuation = TRUE, 
#'				removeNumbers = TRUE, 
#'				removeWords = list(stopwords("english")),
#'				stripWhitespace = TRUE,
#'				stemDocument = TRUE, 
#'				minWordLength = 3,
#'				weighting = weightTf)}
#' @param scoreFUNS scoring functions to be used on \code{TermDocumentMatrix}, if missing defaults to 
#' \code{scoreFUNS = list(
#'				polarity = list(),
#'				subjectivity = list(),
#'				pos_refs_per_ref = list(),
#'				neg_refs_per_ref = list(),
#'				senti_diffs_per_ref = list()
#'		)}
#' @param replace Specifies if existing corpus meta data.frame should be replaced, defaults to TRUE
#' @author Mario Annau
#' @seealso \code{TermDocumentMatrix}
#' @export
`score` <- function(corpus, 
					control, 
					scoreFUNS){
						
	if(missing(control)){
		control = list( 
				tolower = TRUE, 
				removePunctuation = TRUE, 
				removeNumbers = TRUE, 
				removeWords = list(stopwords("english")),
				stripWhitespace = TRUE,
				stemDocument = TRUE, 
				minWordLength = 3,
				weighting = weightTf)
	}
	if(missing(scoreFUNS)){
		scoreFUNS = list(
				polarity = list(),
				subjectivity = list(),
				pos_refs_per_ref = list(),
				neg_refs_per_ref = list(),
				senti_diffs_per_ref = list()
		)
	}

	tdm <- TermDocumentMatrix(corpus, control = control)
	
	
	res <- list()
	for(n in names(scoreFUNS)){
		args <- unlist(scoreFUNS[[n]])
		if(is.null(args)){
			res[[n]] <- eval(call(n, tdm))
		}else{
			res[[n]] <- eval(call(n, tdm, args))
		}
	}
	
	dfres <-  as.data.frame(res)
	meta <- meta(corpus)
    
  for(n in colnames(dfres)) {
    meta(corpus, n) <- dfres[, n]
  }   
	corpus
}

#' Calculate Polarity from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @rdname polarity
#' @export
polarity <- function(x, positive, negative) UseMethod("polarity", x)

#' @rdname polarity
#' @export
polarity.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI() ){
	pos <- tm_term_score(x, positive)
	neg <-  tm_term_score(x, negative)
	all <- (pos+neg)
  ifelse(all != 0, (pos-neg)/all, 0)
}

#' @rdname polarity
#' @export
polarity.DocumentTermMatrix <- function(x, ...) polarity(t(x), ...)

#' Calculate Polarity from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @rdname subjectivity
#' @export
subjectivity <- function(x, positive, negative) UseMethod("subjectivity", x)

#' @rdname subjectivity
#' @export
subjectivity.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI() ){
	pos <- tm_term_score(x, positive)
	neg <-  tm_term_score(x, negative)
	all <- col_sums(x)
  ifelse(all != 0, (pos+neg)/all, 0)
}

#' @rdname subjectivity
#' @export
subjectivity.DocumentTermMatrix <- function(x, ...) subjectivity(t(x), ...)

#' Calculate pos_refs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @rdname pos_refs_per_ref
#' @export
pos_refs_per_ref <- function(x, positive) UseMethod("pos_refs_per_ref", x)

#' @rdname pos_refs_per_ref
#' @export
pos_refs_per_ref.TermDocumentMatrix <- function(x, positive = posterms_GI()){
	pos <- tm_term_score(x, positive)
	all <- col_sums(x)
  ifelse(all != 0, (pos)/(all), 0)
}

#' @rdname pos_refs_per_ref
#' @export
pos_refs_per_ref.DocumentTermMatrix <- function(x, ...) pos_refs_per_ref(t(x), ...)

#' Calculate neg_refs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param negative character vector specifying negative terms to be used, defaults to  \code{\link{negterms_GI}}
#' @rdname neg_refs_per_ref
#' @export
neg_refs_per_ref <- function(x, negative) UseMethod("neg_refs_per_ref", x)

#' @rdname neg_refs_per_ref
#' @export
neg_refs_per_ref.TermDocumentMatrix <- function(x, negative = negterms_GI()){
	neg <- tm_term_score(x, negative)
	all <- col_sums(x)
  ifelse(all != 0, (neg)/(all), 0) 
}

#' @rdname neg_refs_per_ref
#' @export
neg_refs_per_ref.DocumentTermMatrix <- function(x, ...) neg_refs_per_ref(t(x), ...)

#' Calculate senti_diffs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @rdname senti_diffs_per_ref
#' @export
senti_diffs_per_ref <- function(x, positive, negative) UseMethod("senti_diffs_per_ref", x)

#' @rdname senti_diffs_per_ref
#' @export
senti_diffs_per_ref.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI()){
	pos <- tm_term_score(x, positive)
	neg <- tm_term_score(x, negative)
	all <- col_sums(x)
  ifelse(all != 0, (pos-neg)/all, 0)
}

#' @rdname senti_diffs_per_ref
#' @export
senti_diffs_per_ref.DocumentTermMatrix <- function(x, ...) senti_diffs_per_ref(t(x), ...)

#' Get Positive Terms from General Inquirer
#' @author Mario Annau
#' @export
posterms_GI <- function(){
	data("dic_gi")
	data <- get("dic_gi", pos=globalenv()) 
	data[["positive"]]
}
 
#' Get Negative Terms from General Inquirer
#' @author Mario Annau
#' @export
negterms_GI <- function(){
	data("dic_gi")
	data <- get("dic_gi", pos=globalenv()) 
	data[["negative"]]
}