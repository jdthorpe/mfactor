
# ------------------------------------------------
# assignment and subseting
# ------------------------------------------------
#' @export
`[[.mfactor` <- function(x,...){
	# the getter method
	y <- NextMethod("[[")
	attr(y, "levels") <- attr(x, "levels")
	attr(y, "mlevels") <- attr(x, "mlevels")
	class(y) <- oldClass(x)
	y
}

#' @export
#' @method [ mfactor
`[.mfactor` <- function(x,...){
	# the getter method
	y <- NextMethod("[")
	attr(y, "levels") <- attr(x, "levels")
	attr(y, "mlevels") <- attr(x, "mlevels")
	class(y) <- oldClass(x)
	y
}


#' @export
#' @method [<- mfactor
`[<-.mfactor` <- function (x, ..., value) {
	# a utility function
	getlevel <- function(value){
		if(!length(value))
		   return(0)
		if(!all(value %in% lx)) 
			return(NA)
		m <- which(lx %in% value)
		if(length(m) == 1)
			return(m)
		key = paste(sort(unique(m)),collapse=',')
		if(! key %in% attr(x,'mlevels')){
			attr(x,'mlevels') <<- c(attr(x,'mlevels'),key)
		}
		return(match(key,attr(x,'mlevels')) + length(levels(x)))
	}
	cx <- oldClass(x)
	lx <- levels(x)

	if(is.list(value)){
		m <- unlist(lapply(value,getlevel))
	}else if(inherits(value,'mfactor')){
		 if(length(levels(value))){
			 m <- unlist(lapply(as.list(value),getlevel))#this leaves something to be desired. TODO: redo this after implementing the matrix data storage...
		 }else{
			 # value is the 'None' object
			 m <- 0
		 }
	}else{
		m <- integer(0)
		for( i in 1:length(value))
			m[i] <- getlevel(value[i])
	}
	if(any(is.na(m) & !is.na(value))) {
		#browser(skipCalls=500)
		warning("invalid factor level, NAs generated")
	}
	# sript the attributes, assign the new values, then reassign the attributes
	mlx <- attr(x,'mlevels')
	class(x) <- NULL
	attr(x, "levels") <- lx
	attr(x, "mlevels") <- mlx
	x[...] <- m
	class(x) <- cx
	# clean the levels and return the new vector
	return(.cleanLevels(x))
}

#' @export
#' @method [[<- mfactor
`[[<-.mfactor` <- `[<-.mfactor`




#' @export
#' @method $ mfactor
`$.mfactor` <- function(x,level){
	# SYNTACTIC SUGAR
	stopifnot(level %in% levels(x))
	return(as.matrix(x)[,which(level == levels(x))])
}


