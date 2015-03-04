

#' @export
#' @method levels<- mfactor
`levels<-.mfactor` <- function (x, value) 
{
    xlevs <- levels(x)
	if(!length(xlevs))
		return(NextMethod())
	if (length(xlevs) != length(value)) 
		stop("number of levels differs")
	nlevs <- as.character(value)
	if(any(is.na(nlevs)))
		stop('missing values among the levels')
	if (length(unique(nlevs))!= length(nlevs))
		stop("levels are not unique")
    at <- attributes(x)
    at$levels <- nlevs
    attributes(x) <- at
    x
}



