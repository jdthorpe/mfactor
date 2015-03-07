
#' Conversion between lists of values and mfactor variables
#' 
#' Conversion between lists of values and mfactor variables.
#' @param x a list where each element contains zero or more objects from 
#' a unique set of levels. 
#' 
#' @param levels an optional vector of the values (as character strings)
#' that x might have taken. The default is the unique set of values taken by
#' as.character(x), sorted into increasing order of x. Note that this set can
#' be specified as smaller than sort(unique(x)).
#' 
#' @param labels \strong{Either} an optional character vector of labels for the
#' levels (in the same order as 'levels' after removing those in
#' 'exclude'), \strong{or} a character string of length 1.
#' 
#' @param exclude A vector of values to be excluded when forming the set of
#' levels.  NOTE THAT unlike ordinary factors, this parameter defaults to 
#' \code{getOption('mfactor.none','<None>')}.
#' 
#' @param ordered logical flag to determine if the levels should be regarded as
#' ordered (in the order given).
#' 
#' @param ... Additional arguments, which affect the coersion of an x to
#' class mfactor
#' 
#' @export
#' @family Coercion-to-mfactor
#' @inheritParams mfactor
#' @rdname mfactor-list
#' @examples
#' 
#' (x = list(c('a','b','c'),c('c'),c('a','b')))
#' mfactor(x,levels=letters[1:4])
#' 
#' (y = mfactor(c('1,2,3','4,5',6,7,8),split = ',',levels = 1:10))
#' as.list(y)
#' 

mfactor.list <- function(x,
						 labels=levels,
						 levels,
						 ordered=FALSE,
						 exclude,
						 ...){ # additional arguments to mfactor.factor
	modes <- function(x) {
		if(is.null(x))
			NULL
		else if(all(is.na(x))) 
			NULL 
		else 
			mode(x)
	}
	if(length(unique(unlist(lapply(x,modes)))) > 1) stop('all elements must have the same mode')
	y <- unique(unlist(x))
	if(!length(y)) {
		out <- mfactor.factor(factor(),...)
		length(out) <- length(x)
		if(!missing(labels))
			levels(out) <- labels
		out[] <- NONE
		return(out)
	}
	ind <- sort.list(y)
	y <- as.character(y)
	uy <- unique(y[ind]) # observed levels
	uy <- uy[!is.na(uy)]
	missing_levels <- missing(levels)
	if(missing(levels))
		levels <- uy
	if(!missing(exclude))
		levels <- setdiff(levels,exclude)
	if(!missing_levels || !missing(exclude))
		# we need to get rid of the extra levels
		x <- lapply(x,function(x)intersect(x,c(NA,levels)))
#--     if(!missing(levels)) {
#-- 		# HANDLE ANY EXTRA LEVELS IN THE INPUTS NICELY
#-- 		unk <- uy[(!is.na(uy)) & (!uy %in% levels)]
#--         if (length(unk)){
#-- 			if(missing(drop))
#-- 				warning("Invalid factor values (",
#-- 						paste(paste('"',unk,'"',sep = ''),collapse=', ') ,
#-- 						") encountered in input. Invalid values will be dropped."
#-- 						)
#-- 			if (drop) 
#-- 				x <- lapply(x,function(z)(if(any(z %in% levels)) z[z %in% levels] else NA))
#-- 			else {
#-- 				levels <- c(levels,unk)
#-- 				if(!is.null(labels))
#-- 					labels <- c(labels,unk)
#-- 			}
#-- 		}
#-- 	}else{
#-- 		levels <- uy
#--     }
	out <- structure(numeric(0), 
					 mlevels=character(0), 
					 levels=levels,
					 class=c(if(ordered) 'ord_mfactor' ,'mfactor'))

	# the magic happens in `[<-.mfactor`
	out[1:length(x)] <- x
	if(!missing(labels) || !missing_levels){
		if(length(labels) == 1 & length(levels)>1) 
			labels <- paste0(labels,seq(length(levels)))
		if(!length(labels) %in% c(1,length(levels)))
			stop("invalid 'labels'; length ",length(labels), " should be 1 or ",length(levels))
		levels(out)<- labels
	}
	return(out)
}

