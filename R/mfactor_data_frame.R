
# note that as.data.frame.mfactor is intentionally not defined
# if you want indicator variables in a data.frame, use: 
# as.data.frame(as.matrix(mfactor))

#' @export
#' @method mfactor data.frame
mfactor.data.frame <- function(x,...)
	mfactor.matrix(as.matrix(x),...)

