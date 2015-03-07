# ------------------------------------------------------------
# the 'none' element
# ------------------------------------------------------------

#' The None object
#' 
#' The None object in the mfactor package is used to indicate 
#' that none of the elements are selected, which is distinct 
#' from 'NA' which indicates that is is unknown whether or not
#' any of the elements are selected.
#' 
#' @export
#' @rdname None
#' 
NONE <- structure(0,
				  class=c('mfactor'),
				  levels=character(0),
				  mlevels=character(0))


# ----------------------------------------
# DETECTION OF 'NONE' ELEMENTS
# ----------------------------------------

#' @export
#' @rdname None
is.none <- function(x)
	UseMethod("is.none")

#' @export
#' @rdname None
is.none.default <- function(x)
	stop(paste("is.none() is not defined for variables of class '",class(x)[1],"'",sep = ''))

#' @export
#' @rdname None
is.none.ord_mfactor <- function(x)
	(unclass(x) == 0)
#' @export
#' @rdname None
is.none.mfactor <- function(x)
	(unclass(x) == 0)

# ----------------------------------------
# ASSIGNMENT OF 'NONE' ELEMENTS
# ----------------------------------------

#' @export
#' @rdname None
#' @inheritParams base::is.na
`is.none<-` <- function(x,value)
	UseMethod("is.none<-")

#' @export
#' @rdname None
#' @inheritParams base::is.na
`is.none<-.default` <- function(x,value)
	stop(paste("`is.none<-`() is not defined for variables of class '",class(x)[1],"'",sep = ''))

#' @export
#' @method is.none<- mfactor 
#' @rdname None
#' @inheritParams base::is.na
`is.none<-.mfactor` <- function (x, value) {
	oc <- class(x)
	x <- unclass(x)
    x[value] <- 0
	class(x) <- oc
    x
}


