
#' Conversion between lists of values and mfactor variables
#' 
#' Conversion between lists of values and mfactor variables.
#' @param x a list where each entry represents a list of 
#' @export
#' @family mfactor
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
						 ordered=F,
						 exclude,...){ # additional arguments to mfactor.factor
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

