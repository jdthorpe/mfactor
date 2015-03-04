
#' @export
#' @method unique mfactor
unique.mfactor <- function(x,...){
	cx <- oldClass(x)
	class(x) <- NULL
	y <- unique(as.integer(x),...)
	attr(y, "levels") <- attr(x, "levels")
	attr(y, "mlevels") <- attr(x, "mlevels")
	class(y) <- cx
	y
}

