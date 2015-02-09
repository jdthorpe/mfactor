# --------------------------------------------------------------------
# exported methods to other generics
# --------------------------------------------------------------------

# ------------------------------------------------
# primitaves
# ------------------------------------------------

#' @export
#' @method names<- mfactor
`names<-.mfactor` <- function(x,value){
	if(is.null(value)){
		attributes(x)$names <- NULL
		return(invisible(x))
	}
	if(!identical(length(x),length(value)))
		stop(sprintf("'names' attribute [%s] must be the same length as the vector [%s]",
				 length(x),
				 length(value)))
	attr(x,'names') <- value
	invisible(x)
}


#' @export
is.na.mfactor <- function(x)
	is.na(unclass(x)[,1])

#' @export
`is.na<-.mfactor` <- function (x, value) 
{
	ux<- unclass(x)
	ux[value,]<-NA
	attributes(ux)<- attributes(x)
	ux
}
#-- `is.na<-.mfactor` <- function (x, value) 
#-- {
#--	 lx <- levels(x)
#--	 cx <- oldClass(x)
#--	 mlx <- attr(x,'mlevels')
#--	 x <- NextMethod()
#--	 structure(x, levels = lx, class = cx, mlevels = mlx)
#-- }

#' @export
`length<-.mfactor` <- function (x, value) 
{
	out <- vector(x)
	length(out )<- val*length(levels(x))
	at<-attributes(x)
	at$dim[1]  <-  value
	attributes(out)
}

#-- `length<-.mfactor` <- function (x, value) 
#-- {
#--	 lx <- levels(x)
#--	 cx <- oldClass(x)
#--	 mlx <- attr(x,'mlevels')
#--	 x <- NextMethod()
#--	 structure(x, levels = lx, class = cx, mlevels = mlx)
#-- }


#' @export
rep.mfactor <- function (x, ...) 
	x[rep(seq_along(nrow(unclass(x))),...)]

#-- rep.mfactor <- function (x, ...) {
#--	 y <- NextMethod()
#--	 structure(y, class = class(x), levels = levels(x),mlevels = attr(x,'mlevels'))
#-- }

#' @export
#' @method $ mfactor
`$.mfactor` <- function(x,level){
	# SYNTACTIC SUGAR
	stopifnot(level %in% levels(x))
	unclass(x)[,which(level == levels(x))]
}

#-- `$.mfactor` <- function(x,level){
#-- 	# SYNTACTIC SUGAR
#-- 	stopifnot(level %in% levels(x))
#-- 	return(as.matrix(x)[,which(level == levels(x))])
#-- }

# ------------------------------------------------
# generics in package:base
# ------------------------------------------------

#' @export
#' @method dim<- mfactor
`dim<-.mfactor` <- function(...)
   	stop("attribute 'dim' cannot be assigned to objets of class 'mfactor'")

#' @export
#' @method dimnames<- mfactor
`dimnames<-.mfactor` <- function(...)
   	stop("attribute 'dimnames' cannot be assigned to objets of class 'mfactor'")


#' @export
#' @method dim mfactor
dim.mfactor <- function(...)NULL

#' @export
#' @method dimnames mfactor
dimnames.mfactor <- function(...)NULL


# note that this is 'lower case S' Summary
#' @export
#' @method summary mfactor
`summary.mfactor` <- function(x){
	mx <- unclass(x)
	#mx <- as.matrix(x)
	rbind('TRUE' = apply(mx,2,sum,na.rm=T),
		  'FALSE'= apply(!mx,2,sum,na.rm=T),
		  '<NA>' = apply(is.na(mx),2,sum))
}

#' @export
#' @method format mfactor
`format.mfactor` <- base:::format.factor 

#' @export
#' @method duplicated mfactor
duplicated.mfactor <- function(x)
	duplicated(unclass(x))

#' @export
#' @method unique mfactor
unique.mfactor <- function(x,...)
	out <- x[!duplicated(x)]

#' @export
#' @method all.equal mfactor
all.equal.mfactor <- function(x,y)
	all.equal(as.character(x),as.character(y))

#' @export
#' @method length mfactor
length.mfactor <- function(x)
	attributes(x)$dim[1]
	


mm <- matrix(c(T,F,F,
		 F,T,F,
		 F,F,F,
		 T,F,T,
		 NA,NA,NA),ncol=3,byrow=T)
class(mm) <- 'mfactor'
levels(mm) <- letters[1:3]

# --------------------------------------------------
# support for min(),max(), etc. 
# --------------------------------------------------

# note that this is 'upper case S' Summary
#' @export
Summary.mfactor <- function(x,...,na.rm){
	
	if(inherits(x,'ord_mfactor')  )
		ok <- switch(.Generic, min=,max=TRUE, FALSE)
	else
		ok <- FALSE
	if (!ok) 
		stop(.Generic, " not meaningful for ",ifelse(inherits(x,'ordered'),'ordered ','un-ordered'), " mfactors")
	# these could be implemented... then againn perhaps the apply(as.matrix(x),1,any) is more appopriate - I proably shouldn't over-reach with non-intuitive funcionality
	# 'all',  'any'
	FUN <- switch(.Generic,min=base::min,max=base::max)
	as.factor(x,FUN)
}



