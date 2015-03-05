
# --------------------------------------------------
# ordered() family
# --------------------------------------------------

# redefine the base::ordered as a generic.
#' @export
ordered = function (x, ...)
    UseMethod('ordered')

# set the default to the base package
#' @export
#' @method ordered default
ordered.default  <- function(x, ...)
   	base::ordered(x, ...)

# redirect calls to factor.mfactor
#' @export
#' @method ordered mfactor
ordered.mfactor = function(x, ...)
	factor.mfactor(x, ...,ordered=TRUE)

# --------------------------------------------------
# as.ordered() family
# --------------------------------------------------

# redefine the base::as.ordered as a generic.
#' @export as.ordered
as.ordered <- function (x, ...)
    UseMethod('as.ordered')

# set the default to the base package
#' @export
#' @method as.ordered default
as.ordered.default = function(x, ...)
	base::as.ordered(x, ...)


# redirect calls to as.factor.mfactor
#' @export
#' @method as.ordered mfactor
as.ordered.mfactor = function(x, ...)
	as.factor.mfactor(x, ... ,ordered=TRUE)

