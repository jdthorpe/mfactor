
#' S3 generics for \code{\link[base]{factor}},
#' \code{\link[base]{ordered}} ,
#' \code{\link{as.factor}}, and \code{\link{as.ordered}}
#' 
#' @export
#' @rdname factor
#' @param x	a vector of data, usually taking a small number of distinct values. 
#' @param ... additional parameters to be passed to specific methods of \code{\link[base]{factor}},
#' \code{\link[base]{ordered}} or appropriate methods such as 
#' \code{\link{factor.mfactor}} and \code{\link{ordered.mfactor}}
ordered <- function (x, ...)
    UseMethod('ordered')

#' @export
#' @method ordered default
ordered.default  <- function(x, ...)
   	base::ordered(x, ...)

#' @export
#' @rdname mfactor-factor
#' @method ordered mfactor
ordered.mfactor <- function(x, ...)
	factor.mfactor(x, ...,ordered=TRUE)

# --------------------------------------------------
# as.ordered() family
# --------------------------------------------------

#' @rdname factor
#' @export as.ordered
as.ordered <- function (x, ...)
    UseMethod('as.ordered')

#' @export
#' @method as.ordered default
as.ordered.default <- function(x, ...)
	base::as.ordered(x, ...)


#' @export
#' @method as.ordered mfactor
as.ordered.mfactor <- function(x, ...)
	as.factor.mfactor(x, ... ,ordered=TRUE)

