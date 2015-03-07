
#' Test if an object is a multifactor
#' 
#' Test if an object is a multifactor.
#' 
#' @param x An object to be tested
#' 
#' @examples
#' 
#' is.mfactor(1)
#' is.mfactor('a')
#' is.mfactor(mfactor('a'))
#' 
#' @export
is.mfactor <- function (x) 
	inherits(x, "mfactor")

