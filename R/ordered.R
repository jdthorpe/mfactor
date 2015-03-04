
# --------------------------------------------------
# ordered() family
# --------------------------------------------------

# redefine the base::ordered as a generic.
#' @export
ordered = function (...)
    UseMethod('ordered')

# set the default to the base package
#' @export
#' @method ordered default
ordered.default = base::ordered

# redirect calls to factor.mfactor
#' @export
#' @method ordered mfactor
ordered.mfactor = function(x...)
	factor.mfactor(x,...,ordered=T)

# --------------------------------------------------
# as.ordered() family
# --------------------------------------------------

# redefine the base::as.ordered as a generic.
#' @export as.ordered
as.ordered <- function (...)
    UseMethod('as.ordered')

# set the default to the base package
#' @export
#' @method as.ordered default
as.ordered.default = base::as.ordered

# redirect calls to as.factor.mfactor
#' @export
#' @method as.ordered mfactor
as.ordered.mfactor = function(x...)
	as.factor.mfactor(x,...,ordered=T)

