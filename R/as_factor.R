
#' @export as.factor
#' @rdname factor
as.factor <- function (x,...)
    UseMethod('as.factor')

#' @export
#' @method as.factor default 
as.factor.default <- function(x,...)
	base::as.factor(x,...)

