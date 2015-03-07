
#' Recasting of mfactor objects
#' 
#' Recasting of mfactor objects.
#' 
#' @export
#' @inheritParams mfactor.list
#' @family Coercion-to-mfactor
#' @family Coercion-from-mfactor
#' 
#' @examples
#' 
#' (x = mfactor(c('a','b','c')))
#' 
#' # reverse the order of the levels:
#' mfactor(x,levels = c('c','b','a'))
mfactor.mfactor <- function(x,
							levels=attr(x,'levels'),
							labels=levels,
							ordered=inherits(x,'ord_mfactor'),
							...)
	mfactor.list(as.list(x),
				 levels=levels,
				 labels=labels,
				 ordered=ordered,
				 ...)

