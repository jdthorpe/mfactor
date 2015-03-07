
#' Coersion of multi-factors to character vectors
#'
#' @export
#' @method as.character mfactor 
#'
#' @family Coercion-from-mfactor
#'
#' @param x A multi-factor to be coerced to a character vector
#'
#' @param sep A character string used to separate the distinct values that an entry may
#' defaults to ';'
#'
#' @param none A character representation for an element in which none of the levels are
#' taken.  Defaults to \code{'getOption('mfactor.none','<None>')'}.  NOTE that \code{'NONE'}
#' is not the same as NA, the latter representing no information whereas the former indicates 
#' the knowlege that none of levels were indicated.
#'
#' @param ... Additional parameters \code{...} are is not supported
as.character.mfactor <- function (x,
								  sep=';',
								  none= getOption('mfactor.none','<None>') ,
								  ...){
	lx <-  c(attr(x,'levels'),
			 unlist(lapply(lapply(strsplit(attr(x,'mlevels'),','),
								  function(lvl)
									  levels(x)[as.vector(lvl,mode(x))]),
						   paste,collapse=sep))
	)
	stopifnot(length(none)==1)
	if(is.na(none)){
		ux <- unclass(x)
		ux[ux == 0] <- NA
		return(lx[ux])
	}else{
		stopifnot(length(none) == 1)
		stopifnot(inherits(none,'character')  )
		return(c(none,lx)[unclass(x)+1])
	}
}


