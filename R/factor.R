
# redefine the base::factor as a generic.
#' @export
factor = function (...)
    UseMethod('factor')

# set the default to the base package
#' @export
factor.default = base::factor

