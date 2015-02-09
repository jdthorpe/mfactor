# ------------------------------------------------------------
# ------------------------------------------------------------
# conversions to and from ordinary factors
# ------------------------------------------------------------
# ------------------------------------------------------------



# redefine the base::factor as a generic.
#' @export
factor = function (...)
    UseMethod('factor')
# set the default to the base package
factor.default = base::factor

#' Coersion of multi-factors to oridnary factors
#' 
#' Coersion of multi-factors to oridnary factors.
#' 
#' Note that 'base::factor' is *not* an S3 generic, so `factor.mfactor()` must be
#' called directly or `setGenerics()` must be be called prior to calling 
#' `factor()`
#' 
#' @export
#' @rdname mfactor-factor
#' @family mfactor
#' @param x a multi-factor to be coerced to a factor
#' @inheritParams as.character.mfactor
factor.mfactor <- function(x, 
						   levels=attr(x,'levels'), 
						   labels = levels, 
						   exclude,
						   ordered = inherits(x,'ord_mfactor'),
						   ...){
	if(!missing(exclude) && !(is.null(exclude)  | ((length(exclude) == 1) && is.na(exclude)))){
		dflt <- getOption('mfactor.strict.compare',TRUE)
		options(mfactor.strict.compare=FALSE)
		x <- x - exclude
		options(mfactor.strict.compare=dflt)
	}
	as.factor.mfactor(x,labels=labels,levels=levels,ordered=ordered,...)
}

