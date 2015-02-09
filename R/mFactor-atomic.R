# ------------------------------------------------------------
# conversions to atomic types
# ------------------------------------------------------------

#' @export
#' @method mfactor logical
mfactor.logical <- function(x,...)
	mfactor(factor(x,...))

#' @export
#' @method mfactor integer
mfactor.integer <- mfactor.logical

#' @export
#' @method mfactor numeric
mfactor.numeric <- mfactor.logical

#' @export
#' @method mfactor double
mfactor.double <- mfactor.logical

#-- as.vector.mfactor <- function(x,mode='any')
#-- 	switch(mode,
#-- 		   any=,
#-- 		   character=as.vector(as.character.mfactor(x)),
#-- 		   numeric=,
#-- 		   double=as.vector(as.double.mfactor(x)),
#-- 		   integer=as.vector(as.integer.mfactor(x)),
#-- 		   as.vector(x[] <- NA,mode))
#-- 
#-- as.integer.mfactor <-function(x,...) {
#-- 	out <- as.factor.mfactor(x,...)
#-- 	class(out) <- NULL
#-- 	out[out > length(levels(x))] <- NA
#-- 	attributes(out) <- NULL
#-- 	out
#-- }


#' @export
#' @method as.data.frame mfactor
as.data.frame.mfactor <- function(x, 
								 row.names = NULL,
								 optional = FALSE,
								 ...,
								 nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
								 ) 
{
    force(nm)
    nrows <- nrow(unclass(x))
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) == nrows && 
				 !anyDuplicated(row.names)) {
        }
        else row.names <- base::.set_row_names(nrows)
    }
    if(!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}




#-- `as.Date.mfactor` <- base:::as.Date.factor 
#-- 
#-- `as.POSIXlt.mfactor` <- base:::as.POSIXlt.factor 

