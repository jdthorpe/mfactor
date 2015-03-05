
# redefine the base::factor as a generic.
#' @export as.factor
as.factor <- function (x,...)
    UseMethod('as.factor')

# set the default to the base package
#' @export
#' @method as.factor default 
as.factor.default <- function(x,...)
	base::as.factor(x,...)

