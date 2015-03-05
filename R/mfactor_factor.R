
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
						   levels,
						   labels=levels,
						   exclude=getOption('mfactor.none','<None>'),
						   ordered=is.ordered(x),
						   split,
						   ...){# additional arguments to mfactor.character if 'split' specified
	# TODO: split the factor levels without calling mfactor.character
	if('none' %in% names(list(...)))
		stop("argument 'none' deprecated, use 'exclude' instead")
	if(!missing(split)){
		ARGS <- list(x=x,...)
		ARGS[['ordered']] <- ordered
		if(!missing(levels)) 
			ARGS[['levels']] <- levels
		if(!missing(labels)) 
			ARGS[['labels']] <- labels
		if(!missing(exclude)) 
			ARGS[['exclude']] <- exclude
		return(do.call(mfactor.character,ARGS))
	}

	# GIVE A WARNING FOR UNUSED ARGUMENTS
	ARGS <- list(...)
	if(length(ARGS))
		stop(do.call(.unusedArgMessage,ARGS))
	stopifnot(inherits(x,'factor'))

	# HANDLE THE LABELS, IF PROVIDED
	.names <- names(x)
	ux <- unclass(x)


	# HANDLE THE EXCLUSIONS before handling the explicitly passed `levels`
	.levels <- base::levels(x)
	exclude <- intersect(.levels,exclude)
	if(length(exclude)){
		.newLevels <-setdiff(.levels,exclude)
		mch <- match(.levels,.newLevels)
		mch[is.na(mch)] <- 0
		ux <- mch[ux]
		.levels <- .newLevels
		if(missing(levels))
			levels <- setdiff(base::levels(x),exclude)
	}

	# HANDLE EXPLICITLY PASSED LEVELS
	if(!missing(levels)){
		notMissing <- (is.na(ux)) | (ux > 0)
		ux[notMissing] <- match(.levels,levels)[ux[notMissing]]
	} else {
		levels <- .levels
	}

	# MIMMIC THE HANDLING OF THE LABELS ARGUMENT IN `FACTOR()`
	if(length(labels) == 1 & length(levels) >1) 
		labels <- paste(labels,seq(length(levels)),sep = '')
	if(!length(labels) %in% c(1,length(levels)))
		stop("invalid 'labels'; length ",length(labels), " should be 1 or ",length(levels))

	stopifnot(all(ux >= 0,na.rm=TRUE))
	stopifnot(all(ux <= length(labels),na.rm=TRUE))
	y <- structure(ux, mlevels=character(0), levels= labels)
	if(!is.null(.names)) 
		names(y) <- .names
	class(y) <- c(if (ordered) "ord_mfactor", "mfactor")
	y
}

