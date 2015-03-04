
#' @export
#' @rdname mfactor-factor
#' @method as.factor mfactor 
#' @family mfactor
#' @inheritParams factor.mfactor
#' @inheritParams as.character.mfactor
as.factor.mfactor <- function(x,
							  collapse, # argument for as.character.mfactor
							  sep=getOption('mfactor.sep',';'),# argument for as.character.mfactor
							  ...,# additional arguments to as.character.mfactor
									# -or- to collapse
							  none=if(any((!is.na(unclass(x))) & unclass(x) == 0)) 
									  getOption('mfactor.none','<None>') 
								  else NA){
	stopifnot(length(none) == 1) # prevents none=NULL as well as longer values
	ord <- inherits(x,'ord_mfactor')

	if(missing(collapse) & missing(sep)){
		tmp = getOption('mfactor.collapse')
		if(!is.null(tmp))
			collapse <- tmp
	}
		
	# --------------------------------------------------
	# fast path out when  'collapse' is not specified
	# --------------------------------------------------
	if(missing(collapse)){
		# return a factor with all the data
		cx <- as.character.mfactor(x,sep=sep,none=none,...)
		lx <-  c(attr(x,'levels'),
				 unlist(lapply(lapply(strsplit(attr(x,'mlevels'),','),
									  function(lvl)levels(x)[as.vector(lvl,mode(x))]),paste,collapse=sep)))
		if(!is.na(none) && (!none %in% lx))
			lx <- c(none,lx)
		return(factor(cx,levels=lx,ordered=ord))
	}

	# --------------------------------------------------
	# Raise an error if there extra arguments and no
	# function to absorb them
	# --------------------------------------------------
	if (mode(collapse) != 'function'){
		# ADDITIONAL ARGUMENTS ARE ONLY FOR USER SUPPLIED FUNCTIONS...
		ARGS <- list(...)
		if(length(ARGS))
			stop(do.call(.unusedArgMessage,ARGS))
	}

	ox <- x
	lx <- levels(x)
	mlx <- attr(x,'mlevels') 
	cx <- class(x)
	class(x) <- NULL
	# --------------------------------------------------
	# PROCESS THE MULTIPLY VALUED ELEMENTS 
	# --------------------------------------------------
	if(any((x>length(lx)) & !is.na(x))){
		if (mode(collapse) == "function"){
			altx <- lapply(lapply(strsplit(mlx,','),as.vector,'integer'),
							function(x,...){
								if (is.null(x)||length(x) == 0)
									NA
								else
									collapse(x,...)
							},...)
			if(!all(unlist(lapply(altx,length)) ==1))
				stop('Bad user defined collapse function: collapse(...) must return integers of length 1')
			altx <- unlist(altx)
			altx[is.na(altx) | altx != as.integer(altx)] <- NA
			x <- c(0:length(lx),altx)[x+1]

			# ------------------------------------------------------------
			# warn the user if missing values are created by coersion
			# ------------------------------------------------------------
			# An indictor that he new vecor is NA, and is not expected to be NA.
			# We expect the the new vecror to be NA if the original is NA 
			# or if the original is emtpy and we're seting empties to NA.
			newNAs <- is.na(x) & !is.na(ox)
			if(any(newNAs))
				warning('NAs introduced by coersion from levels:',
						paste('"',as.character(unique(ox[newNAs])),'"',sep = '',collapse = ', '))


		} else if(is.na(collapse)){
			x[x>length(lx)] <- NA
		} else if(inherits(collapse,'character') && length(collapse) == 1){
			x[x>length(lx)] <- length(lx) + 1
			lx <- c(lx,collapse)
		} else stop("'invalid value for argument 'collapse'" )
	} 
	# --------------------------------------------------
	# PROCESS THE ELEMENTS TTHAT DON'T TAKE A VALUE
	# --------------------------------------------------
	if(is.na(none)){
		x[x == 0] <- NA
	}else{
		stopifnot(inherits(none,'character') && length(none) == 1 )
		if(none %in% lx){
			x[x == 0] <- which(lx == none)
		}else{
			x <- x + 1
			lx <- c(none,lx)
		}
	}

	# ------------------------------------------------------------
	# create the output 
	# ------------------------------------------------------------
	#class(x) <- NULL
	x <- as.integer(x)
	levels(x) <- lx
	class(x) <- c(if (ord) 'ordered','factor')
	return(x)
}



