# ------------------------------------------------------------
# the 'none' element
# ------------------------------------------------------------

#' @export
NONE <- structure(0,class=c('mfactor'),levels=character(0),mlevels=character(0))


# ----------------------------------------
# DETECTION OF 'NONE' ELEMENTS
# ----------------------------------------

#' @export
is.none <- function(x)
	UseMethod("is.none")

#' @export
is.none.default <- function(x,...)
	stop(paste("is.none() is not defined for variables of class '",class(x)[1],"'",sep = ''))

#' @export
is.none.ord_mfactor <- function(x)
	(unclass(x) == 0)
#' @export
is.none.mfactor <- function(x)
	(unclass(x) == 0)

# ----------------------------------------
# ASSIGNMENT OF 'NONE' ELEMENTS
# ----------------------------------------

#' @export
`is.none<-` <- function(x,...)
	UseMethod("is.none<-")

#' @export
`is.none<-.default` <- function(x,...)
	stop(paste("`is.none<-`() is not defined for variables of class '",class(x)[1],"'",sep = ''))

#' @export
#' @method is.none<- mfactor 
`is.none<-.mfactor` <- function (x, value) {
	oc <- class(x)
	x <- unclass(x)
    x[value] <- 0
	class(x) <- oc
    x
}


