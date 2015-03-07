
#' @export
#' @rdname factor
factor = function (x, ...)
    UseMethod('factor')

#' @export
factor.default  <- function(x, ...) 
	base::factor(x, ...)

