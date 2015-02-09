# ------------------------------------------------------------
# conversions to and from indicator matricies
# ------------------------------------------------------------

#' @export
#' @method names<- mfactor
`names<-.mfactor` <- function(x,value){
	if(is.null(value))
		attributes(x)$names <- NULL
	if(!identical(length(x),length(value)))
		stop(sprintf("'names' attribute [%s] must be the same length as the vector [%s]",
				 length(x),
				 length(value)))
	attributes(x)$names <- value
	invisible(x)
}

#' @export
#' @rdname mfactor-matrix
#' @family mfactor
as.matrix.mfactor <- function(x){
	# convert to a matrix of indicator variables
	out <- unclass(x)
	attributes(out) <- NULL
	dim(out) <- dim(unclass(x))
	colnames(out) <- levels(x)
	if(!is.null(names(x)))
		rownames(out) <- names(x)
	out
}



