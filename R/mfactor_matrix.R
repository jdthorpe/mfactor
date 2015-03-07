
#' Conversion between indicator matricies and mfactor variables
#' 
#' Conversion between indicator matricies and mfactor variables.
#' @export
#' @param x A matrix of mode logical to be coerced to a vector with class multi-factor
#' or a multifactor to be coerced to matrix. 
#' 
#' @param levels a vector of levls with length equal to \code{ncol(x)}
#' to be the levels of the resulting mfactor. (default = dimnames(x)[[2]])
#' 
#' @param ... Additional arguments, which affect the coersion of an x to
#' class mfactor
#' 
#' @rdname mfactor-matrix
#' @family Coercion-to-mfactor
#' @examples
#' 
#' (x = matrix(
#'    c(TRUE ,TRUE ,TRUE ,FALSE,
#'      FALSE,FALSE,TRUE ,FALSE,
#'      TRUE ,TRUE ,FALSE,FALSE),
#'    nrow=3,
#'    dimnames = list(1:3,letters[1:4])))
#' mfactor(x)
#'  
#' (y = mfactor(c('1,2,3','4,5',6,7,8),split = ',',levels = 1:10))
#' as.matrix(y)
#' 
mfactor.matrix <- function(x,levels=dimnames(x)[[2]],...){
	stopifnot(mode(x) == 'logical')
	stopifnot(length(levels) == ncol(x))
	m <- as.list(apply(x,1,which))
	m <- lapply(m,function(x)levels[x])
	m[apply(x,1,function(g)any(is.na(g)))] <- NA
	m <- mfactor.list(m,levels=levels,...)
	names(m) <- dimnames(x)[[1]]
	m
}



