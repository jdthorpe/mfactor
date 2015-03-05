
#' @export
#' @method all.equal mfactor
all.equal.mfactor <- function(target,current,...){
	target = as.character(target)
	current  = as.character(current)
	NextMethod(`all.equal`)
}

