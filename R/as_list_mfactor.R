
#' @export
#' @family mfactor
#' @rdname mfactor-list
as.list.mfactor <- function(x,levels=FALSE, ...) {
	ARGS <- list(...)
	if(length(ARGS))
		stop(do.call(.unusedArgMessage,ARGS))
	# convert to a list with the factor levels where length(out) == length(x)
	lx <- attr(x,'levels')
	mlx <- attr(x,'mlevels')
	class(x) <- NULL
	out <- list()
	for(i in 1:length(x)){
		if(is.na(x[i])){
			out[i] <- list(NA)
			next
		}
		if(x[i] == 0){
			out[i] <- list(integer(0))
			next
		}
		if(x[i] <= length(lx)){
			if(levels)
				out[[i]] <- x[i] 
			else
				out[[i]] <-  lx[x[i]]
			next
		}
		if(x[i] - length(lx) <= length(mlx)){
			key <-  mlx[x[i] - length(lx)]
			keys <- as.numeric(strsplit(key,',')[[1]])
			if(levels)
				out[[i]] <- as.integer(keys) 
			else
				out[[i]] <- lx[keys]
			next
		}
		out[i] <- list(NULL)
	}
	return(out)
}

