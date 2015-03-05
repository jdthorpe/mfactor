
# redefine the base::factor as a generic.
#' @export
factor = function (x, ...)
    UseMethod('factor')

# set the default to the base package
#' @export
factor.default  <- function(x, ...) 
	base::factor(x, ...)

