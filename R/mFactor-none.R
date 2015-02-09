
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
	apply(!unclass(x),1,all)
	#(unclass(x) == 0)

#' @export
is.none.mfactor <- function(x)
	apply(!unclass(x),1,all)
	#(unclass(x) == 0)

# ----------------------------------------
# ASSIGNMENT OF 'NONE' ELEMENTS
# ----------------------------------------

#' @export
`is.none<-` <- function(x,...)
	UseMethod("is.none<-")

#' @export
#' @method is.none<- default 
`is.none<-.default` <- function(x,...)
	stop(paste("`is.none<-`() is not defined for variables of class '",class(x)[1],"'",sep = ''))

#' @export
#' @method is.none<- mfactor 
`is.none<-.mfactor` <- function (x, value) {
	out<-uncalss(x)
	out[,Value] <- F
	attributes(out) <- attributes(x)
    out
}

NONE <- structure(matrix(F),class=c('mfactor'),levels=character(0))

#' @export
NONE 


