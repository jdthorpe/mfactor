
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


	# HANDLE THE EXCLUSIONS before handling the explicitly passed `levels`
	if(!missing(exclude) && !(is.null(exclude)  || all(is.na(exclude)))){
		dflt <- getOption('mfactor.strict.compare',TRUE)
		on.exit(options(mfactor.strict.compare=dflt))
		options(mfactor.strict.compare=FALSE)
		if(is.list(exclude))
			x <- x - exclude
		else
			x <- x - list(exclude)
		# don't mess with explicitly passed levels
		if(missing(levels))
			levels <- setdiff(levels,exclude)
	}


	if(!missing(labels) || !missing(levels)|| !missing(exclude))
		x <- mfactor(as.list(x),labels=labels,levels=levels)

	if(!missing(ordered))
		class(x) <- c(if (ordered) "ord_mfactor", "mfactor")

	as.factor.mfactor(x,...)

}

