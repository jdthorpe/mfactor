# ------------------------------------------------------------
# conversions to atomic types
# ------------------------------------------------------------

#' @export
#' @method mfactor logical
mfactor.logical <- function(x,...)
	mfactor(factor(x,...))

#' @export
#' @method mfactor integer
mfactor.integer <- mfactor.logical

#' @export
#' @method mfactor numeric
mfactor.numeric <- mfactor.logical

#' @export
#' @method mfactor double
mfactor.double <- mfactor.logical

#' @export
#' @method as.vector mfactor 
as.vector.mfactor <- function(x,mode='any')
	switch(mode,
		   any=,
		   character=as.vector(as.character.mfactor(x)),
		   numeric=,
		   double=as.vector(as.double.mfactor(x)),
		   integer=as.vector(as.integer.mfactor(x)),
		   as.vector(x[] <- NA,mode))

#' @export
#' @method as.integer mfactor 
as.integer.mfactor <-function(x,...) {
	out <- as.factor.mfactor(x,...)
	class(out) <- NULL
	out[out > length(levels(x))] <- NA
	attributes(out) <- NULL
	out
}

#' @export
#' @method as.double mfactor 
as.double.mfactor <- as.integer.mfactor

#' @export
#' @method as.data.frame mfactor
as.data.frame.mfactor <- base:::as.data.frame.factor 

#' @export
#' @method as.Date mfactor
`as.Date.mfactor` <- base:::as.Date.factor 

#' @export
#' @method as.POSIXlt mfactor
`as.POSIXlt.mfactor` <- base:::as.POSIXlt.factor 

