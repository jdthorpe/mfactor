# -------------------------------------------------------------------
# A functions defining a novel multi-value or "subset like" factor data type
# Author: Jason Thorpe
# -------------------------------------------------------------------

# --------------------------------------------------
# STILL TO DO: 
# --------------------------------------------------
#
# (1) implement with an underlying matrix, rather than the (sparse) factor 
#     like codes  which have to be parsed, and allow for the possibility 
#     to have some levels of a value missing but not others, as in :
#
#			> mfactor(matrix(c(NA,TRUE,FALSE,TRUE),nrow=2),levels=c('a','b'))
#			[1] <NA:a>;b a;b 
#			Levels: a b


#' Multi-level Factors
#'
#' The function \code{mfactor} is used to create a factor like vector in which 
#' individual entries may take zero or more values from the unique levels of x 
#' (\code{levels(x)}). 
#'
#' @param x An object to be coerced into a multi-factor
#' levels an optional vector of the values (as character strings) that
#' 'x' might have taken.  The default is the unique set of
#' values taken by \code{'as.character(strsplit(x,split,...))'} if the 
#' argument \code{'split'} is specified and \code{'as.character(x)'} otherwise, 
#' sorted into increasing order _of 'x'_.  Note that the value of this argument 
#' does not have to include all possible values of x.
#' 
#' @param ... Additional arguments, which affect the coersion of an x to
#' class mfactor
#' 
#' @export
#' @rdname mfactor
#' @family Coercion-to-mfactor
#' @family Coercion-from-mfactor
#' @example man/mFactor-examples.r
mfactor <- function (x, ...) 
	UseMethod("mfactor")

#' @export
mfactor.default <- function(x,...){# additional arguments to mfactor.character
	if(all(is.na(x))) 
		return(mfactor.character(as.character(x),...))
	else
		paste('I dont know how to coerce a class',class(x)[1],'variable to an mfactor')
}

#' @export
mfactor.NULL <- function(x,...)
		return(mfactor.character(x,...))



