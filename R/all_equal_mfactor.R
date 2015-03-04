
#' @export
#' @method all.equal mfactor
all.equal.mfactor <- function(x,y)
	all.equal(as.character(x),as.character(y))

