
# redefine the base::ordered as a generic.
#' @export
ordered = function (...)
    UseMethod('ordered')

# set the default to the base package
ordered.default = base::ordered

# redirect calls to factor.mfactor
ordered.mfactor = function(x...)
	factor.mfactor(x,...,ordered=T)

# redefine the base::as.ordered as a generic.
#' @export
as.ordered = function (...)
    UseMethod('as.ordered')

# set the default to the base package
as.ordered.default = base::as.ordered

# redirect calls to as.factor.mfactor
as.ordered.mfactor = function(x...)
	as.factor.mfactor(x,...,ordered=T)

