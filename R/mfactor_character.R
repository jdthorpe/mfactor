
#' Multi-level Factors
#'
#' The function \code{mfactor} is used to create a factor like vector in which 
#' individual entries may take zero or more values from the unique levels of x 
#' (\code{levels(x)}). 
#'
#' mfactors are particularly useful for transforming delimited text vectors 
#' (e.g. ) and converting it to a more useable format such as a \code{matrix} 
#' or a set of indicator variables.  (See examples)
#'
#' @param x An object to be coerced into a multi-factor
#' @param ... Additional arguments, which affect the coersion of an x to
#' class mfactor
#' 
#'
#' @export
#' @family Coercion-to-mfactor
#' @inheritParams base::strsplit
#' @inheritParams mfactor
#' @examples
#' 
#' (x = c("a,b,c","c","a,b"))
#' mfactor(x,levels=letters[1:4],split = ',')
#'  
#' (y = mfactor(c('1,2,3','4,5',6,7,8),split = ',',levels = 1:10))
#' as.character(y,sep = ',')
mfactor.character <- function(x=character(),
							  split,
							  fixed = FALSE,
							  perl = FALSE,
							  useBytes = FALSE,
							  ...){# additoinal argumnts to mfactor.list -or- factor()
	if(!missing(split)){
		x <- strsplit(x,split,fixed,perl,useBytes)
		#x <- lapply(x,function(x) if(length(x)) x else NA)
		ARGS <- list(x=x,...)
		if('exclude' %in% names(ARGS)){
		   	x <- lapply(x,function(x)x[!x %in% ARGS[['exclude']]])
			ARGS <- ARGS[-which('exclude' %in% names(ARGS))]
		}
		do.call(mfactor.list,ARGS)
	} else
		mfactor.factor(factor(x,...))
}
