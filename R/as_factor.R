
# redefine the base::factor as a generic.
#' @export as.factor
as.factor <- function (...)
    UseMethod('as.factor')

# set the default to the base package
#' @export
#' @method as.factor default 
as.factor.default = base::as.factor

