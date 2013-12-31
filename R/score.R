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
					scoreFUNS,
					replace = TRUE){
						
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
	meta <- DMetaData(corpus)

	if(replace){
		MetaID <- meta$MetaID
		#DMetaData(corpus) <- cbind(MetaID, dfres)
		meta[,colnames(dfres)] <- dfres
		DMetaData(corpus) <- meta
	}else{
		DMetaData(corpus) <- cbind(DMetaData(corpus), dfres)
	}
	corpus
}

#' Calculate Polarity from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @S3method polarity TermDocumentMatrix
#' @S3method polarity DocumentTermMatrix
#' @export
polarity <- function(x, positive, negative) UseMethod("polarity", x)
polarity.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI() ){
	pos <- tm_tag_score(x, positive)
	neg <-  tm_tag_score(x, negative)
	(pos-neg)/(pos+neg)
}
polarity.DocumentTermMatrix <- function(x, ...) polarity(t(x), ...)

#' Calculate Polarity from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @S3method polarity TermDocumentMatrix
#' @S3method polarity DocumentTermMatrix
#' @export
subjectivity <- function(x, positive, negative) UseMethod("subjectivity", x)
subjectivity.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI() ){
	pos <- tm_tag_score(x, positive)
	neg <-  tm_tag_score(x, negative)
	all <- col_sums(x)
	(pos+neg)/all
}
subjectivity.DocumentTermMatrix <- function(x, ...) subjectivity(t(x), ...)

#' Calculate pos_refs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @S3method pos_refs_per_ref TermDocumentMatrix
#' @S3method pos_refs_per_ref DocumentTermMatrix
#' @export
pos_refs_per_ref <- function(x, positive) UseMethod("pos_refs_per_ref", x)
pos_refs_per_ref.TermDocumentMatrix <- function(x, positive = posterms_GI()){
	pos <- tm_tag_score(x, positive)
	all <- col_sums(x)
	(pos)/(all)
}
pos_refs_per_ref.DocumentTermMatrix <- function(x, ...) pos_refs_per_ref(t(x), ...)

#' Calculate neg_refs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param negative character vector specifying negative terms to be used, defaults to  \code{\link{negterms_GI}}
#' @S3method neg_refs_per_ref TermDocumentMatrix
#' @S3method neg_refs_per_ref DocumentTermMatrix
#' @export
neg_refs_per_ref <- function(x, negative) UseMethod("neg_refs_per_ref", x)
neg_refs_per_ref.TermDocumentMatrix <- function(x, negative = negterms_GI()){
	neg <- tm_tag_score(x, negative)
	all <- col_sums(x)
	(neg)/(all)
}
neg_refs_per_ref.DocumentTermMatrix <- function(x, ...) neg_refs_per_ref(t(x), ...)

#' Calculate senti_diffs_per_ref from Matrix
#' @author Mario Annau
#' @param x TermDocumentMatrix
#' @param positive character vector specifying positive terms to be used, defaults to \code{\link{posterms_GI}}
#' @param negative character vector specifying negative terms to be used, defaults to\code{\link{negterms_GI}}
#' @S3method senti_diffs_per_ref TermDocumentMatrix
#' @S3method senti_diffs_per_ref DocumentTermMatrix
#' @export
senti_diffs_per_ref <- function(x, positive, negative) UseMethod("senti_diffs_per_ref", x)
senti_diffs_per_ref.TermDocumentMatrix <- function(x, positive = posterms_GI(), negative = negterms_GI()){
	pos <- tm_tag_score(x, positive)
	neg <- tm_tag_score(x, negative)
	all <- col_sums(x)
	(pos-neg)/all
}
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