
# redefine the base::factor as a generic.
#' @export
as.factor = function (...)
    UseMethod('as.factor')

# set the default to the base package
as.factor.default = base::as.factor

#' @export
#' @rdname mfactor-factor
#' @method as.factor mfactor 
#' @family mfactor
#' @inheritParams factor.mfactor
#' @inheritParams as.character.mfactor
as.factor.mfactor <- function(x,

							  if sep is provided, it should override collapse 
							  if sep is provided, it should override collapse 
							  if sep is provided, it should override collapse 

								also perhaps collapse should be renamed multiple or something.
								also perhaps collapse should be renamed multiple or something.
								also perhaps collapse should be renamed multiple or something.

							  collapse=getOption('mfactor.collapse','<Multiple.Values>'), # argument for as.character.mfactor
							  sep=getOption('mfactor.sep',';'),# argument for as.character.mfactor
							  ...,# additional arguments to as.character.mfactor
									# -or- to collapse
							  none= getOption('mfactor.none','<None>') ){
	stopifnot(length(none) == 1) # prevents none=NULL as well as longer values
	ord <- inherits(x,'ord_mfactor')

	if(missing(collapse) & missing(sep)){
		tmp = getOption('mfactor.collapse')
		if(!is.null(tmp))
			collapse <- tmp
	}
		
	# --------------------------------------------------
	# fast path out when  'collapse' is NULL
	# --------------------------------------------------
	if(is.null(collapse)){
		# return a factor with all the data
		cx <- as.character.mfactor(x,sep=sep,none=none,...)
		lx<-levels(x)
		if(!is.na(none) && (!none %in% lx))
			lx <- c(none,lx)
		our <-factor(cx,
					 levels=c(lx,setdiff(unique(cx),lx)),
					 ordered=ord)
		return(out)
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

	# --------------------------------------------------
	# PROCESS THE MULTIPLY VALUED ELEMENTS 
	# --------------------------------------------------
	if (mode(collapse) == "function"){
		out <- apply(unclass(x),
					  1,
					  function(y,...){
							if (ia.na(y[1]))
								collapse(NA,...)
							else
								collapse(which(y),...)
						},
						...)
		if(is.list(out)||is.matrix(out)||mode(out)!='integer')
			stop('Bad user defined collapse function: collapse(...) must return integers of length 1')

		attributes(out)<-attributes(factor(NA,levels=levels(x)))

		# ------------------------------------------------------------
		# warn the user if missing values are created by coersion
		# ------------------------------------------------------------
		# An indictor that he new vecor is NA, and is not expected to be NA.
		# We expect the the new vecror to be NA if the original is NA 
		# or if the original is emtpy and we're seting empties to NA.
		newNAs <- is.na(out) & !is.na(x)
		if(any(newNAs))
			warning('NAs introduced by coersion from levels:',
					paste('"',as.character(unique(x[newNAs])),
						  '"',
						  sep = '',
						  collapse = ', '))

		return(out)

	} 

	# PREP THE OUTPUT
	out <- rep_len(NA_integer_,nrow(unclass(x)))
	lx<-levels(x)
	browser()
	
	.rowSums <- rowSums(unclass(x))
	.missing <- is.na(.rowSums)

	# --------------------------------------------------
	# PROCESS THE ELEMENTS with exactly one value
	# --------------------------------------------------
	singles <-  .rowSums==1
	singles[.missing] <- F
	out[singles] <- apply(unclass(x)[singles,], 1,which)

	# --------------------------------------------------
	# PROCESS THE ELEMENTS THAT with multiple VALUEs
	# --------------------------------------------------

	mutiples <-  .rowSums>1
	mutiples[.missing] <- F

	if(is.na(collapse)){
		out[multiples] <- NA
	} else if(inherits(collapse,'character') && length(collapse) == 1){
		lx <- c(lx,collapse)
		out[mutiples] <- length(lx) 
	} else stop("'invalid value for argument 'collapse'" )


	# --------------------------------------------------
	# PROCESS THE ELEMENTS THAT DON'T TAKE A VALUE
	# --------------------------------------------------
	# identify records with zero values
	zeros <- .rowSums == 0
	zeros[.missing] <- F

	if(is.na(none)){
		out[zeros] <- NA
	}else{
		stopifnot(inherits(none,'character') && length(none) == 1 )
		if(none %in% lx){
			out[zeros] <- which(lx == none)
		}else{
			out <- out + 1
			lx <- c(none,lx)
		}
	}

	# ------------------------------------------------------------
	# create the output 
	# ------------------------------------------------------------
	out <- as.integer(out)
	attributes(out) <- attributes(factor(NA,levels=lx,ordered=ord))
	out
}



