# ------------------------------------------------------------
# ------------------------------------------------------------
# conversions to and from ordinary factors
# ------------------------------------------------------------
# ------------------------------------------------------------

#' Coersion of factors to Multi-level Factors 
#'
#' The function \code{mfactor} is used to create a factor like vector in which 
#' individual entries may take zero or more values from the unique levels of x 
#' (\code{levels(x)}). 
#'
#' @export
#' @family mfactor
#' @rdname mfactor-factor
#' @param x a factor to be coerced to a multi-factor
#' @param split if specified, causes x to be coerced by \code{\link{as.character.mfactor}}
#' @param ...  additional arguments depending on wether 'split' is specified
#' @inheritParams mfactor
mfactor.factor <- function(x,
						   levels=base::levels(x),
						   labels,
						   ordered=is.ordered(x),
						   exclude=getOption('mfactor.none','<None>'),
						   split,
						   ...){
	# ... are additional arguments to mfactor.character if 'split' specified
	if('none' %in% names(list(...)))
		stop("argument 'none' deprecated, use 'exclude' instead")
	stopifnot(inherits(x,'factor'))

	if(!missing(split)){
		ARGS <- list(x=levels(x),...)
		ARGS[['ordered']] <- ordered
		if(!missing(levels)) 
			ARGS[['levels']] <- levels
		if(!missing(labels)) 
			ARGS[['labels']] <- labels
		if(!missing(exclude)) 
			ARGS[['exclude']] <- exclude
		temp <-do.call(mfactor.character,ARGS)
		out<-unclass(temp)[unclass(x),]
		at <- attributes(temp)
		at$dim[1] <- length(x)
		attributes(out) <- at
		return(out)
	}

	# GIVE A WARNING FOR UNUSED ARGUMENTS
	ARGS <- list(...)
	if(length(ARGS))
		stop(do.call(.unusedArgMessage,ARGS))

	# HANDLE THE LABELS, IF PROVIDED
	.levels <- base::levels(x)
	.names <- names(x)
	ux <- unclass(x)

	# HANDLE THE EXCLUSIONS
	exclude <- intersect(.levels,exclude)
	if(length(exclude)){
		.newLevels <-setdiff(.levels,exclude)
		mch <- match(.levels,.newLevels)
		mch[is.na(mch)] <- 0
		ux <- mch[ux]
		.levels <- .newLevels
	}

	# handle the labels
	if(missing(labels)) 
		labels <- .levels

	if(length(labels) == 1 & length(levels) >1) 
		labels <- paste(labels,seq(length(.levels)),sep = '')
	if(!length(labels) %in% c(1,length(.levels)))
		stop("invalid 'labels'; length ",length(labels), " should be 1 or ",length(.levels))

	stopifnot(all(ux >= 0,na.rm=T))
	stopifnot(all(ux <= length(labels),na.rm=T))

	# creat the underlying matrix
	mx <- matrix(F,length(x),length(.levels))
	mx[!is.na(ux) & ux==0,] <- F
	mx[cbind(seq_along(ux),ux)] <- T

	# give it a structure
	out <- structure(mx, 
					 levels = labels,
					 class = c(if (ordered) "ord_mfactor", "mfactor"),
					 names = .names)

}



