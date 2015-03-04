
#' @export
#' @rdname mfactor
mfactor.mfactor <- function(x,levels=attr(x,'levels'),labels=levels,ordered=inherits(x,'ord_mfactor'),...)
	mfactor.list(as.list(x),levels=levels,labels=labels,ordered=ordered,...)

