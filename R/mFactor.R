# -------------------------------------------------------------------
# A functions defining a novel multi-value or "subset like" factor data type
# Author: Jason Thorpe
# Date: Thursday 11/10/2011 05:09 PM
# -------------------------------------------------------------------
#
# the multi-factor is much like a factor type variable which takes
# on a fixed set of values, but unlike factors, each element in a 
# multi-factor can take on any subset of the possible levels not
# just one
#
# this was inspired by the fact that I have to deal with lots of survey 
# data where individuals are asked to "pick the option that best describes
# you" and they pick multiple options.  Having to clean that data is a pain, 
# so these give a nice interface to that type of data, and the "as.factor"
# method give a quick way convert to the more widely usable factor types 
# (eg with lm(), glm(), etc. )

# see  ?InternalMethods for complete list of generics

# --------------------------------------------------
# STILL TO DO: 
# --------------------------------------------------
#
# (0) make sure the signatures of all the methods are compatible and that
#     the '...' argument is used consistently from call to call (a call 
#     diagram may be helpful...)
#
# (1) implement with an underlying matrix, rather than the silly factor 
#     like codes  which have to be parsed, and allow for the possibility 
#     to have some levels of a value missing but not others, as in :
#
#			> mfactor(matrix(c(NA,T,F,T),nrow=2),levels=c('a','b'))
#			[1] <NA:a>;b a;b 
#			Levels: a b
#
# (2) implement a better as.data.frame.mfactor
#
# (3) complete the documentation
#
# (4) make a better attmept to deterime if base::factor is a generic or not in .onLoad()
# 
# (5) make a vingette for this package
# 

# ------------------------------
# because the empty string is interpreted as the emtpy value 
# then some SOME THINGS TO NOTE: 
# ------------------------------
#-- # note that this is not currently true, but would be true 
#-- # THESE ARE NOT THE SAME
#-- levels(mfactor(c('','a'),none=""))
#-- levels(factor(c('','a')))
#-- 
#-- # THESE ARE THE SAME
#-- levels(mfactor(c('','a')))
#-- levels(factor(c(NA,'a')))
#-- 
#-- # BUT THESE ARE NOT
#-- as.character(mfactor(c('','a')))
#-- as.character(factor(c(NA,'a')))


# note that as.data.frame.mfactor is intentionally not defined
# it is borrowed from base:::as.data.frame.factor
# if you want indicator variables in a data.frame, use: 
# as.data.frame(as.matrix(mfactor))


# ------------------------------------------------
# mfactor Generics + defualt
# ------------------------------------------------

#' Multi-level Factors
#'
#' The function \code{mfactor} is used to create a factor like vector in which 
#' individual entries may take zero or more values from the unique levels of x 
#' (\code{levels(x)}). 
#'
#' @param x An object to be coerced into a multi-factor
#' levels an optional vector of the values (as character strings) that
#' 'x' might have taken.  The default is the unique set of
#' values taken by \code{'as.character(strsplit(x,split,...))'} if the 
#' argument \code{'split'} is specified and \code{'as.character(x)'} otherwise, 
#' sorted into increasing order _of 'x'_.  Note that the value of this argument 
#' does not have to include all possible values of x.
#' 
#' @param labels \strong{Either} an optional character vector of labels for the
#' levels (in the same order as 'levels' after removing those in
#' 'exclude'), \strong{or} a character string of length 1.
#' 
#' @param exclude A vector of values to be excluded when forming the set of
#' levels.  NOTE THAT unlike ordinary factors, this parameter defaults to 
#' \code{getOption('mfactor.none','<None>')}.
#' 
#' @param ordered logical flag to determine if the levels should be regarded as
#' ordered (in the order given).
#' @param ... additional arguments, depending on the value of x.  
#' @export
#' @family mfactor
mfactor <- function (x, ...) 
	UseMethod("mfactor")

#' @export
is.mfactor <- function (x) 
	inherits(x, "mfactor")

#' @export
mfactor.default <- function(x,...){# additional arguments to mfactor.character
	if(all(is.na(x))) 
		return(mfactor.character(as.character(x),...))
	else
		paste('I dont know how to coerce a class',class(x)[1],'variable to an mfactor')
}


#' @export
#' @rdname mfactor
mfactor.mfactor <- function(x,levels=attr(x,'levels'),labels=levels,ordered=inherits(x,'ord_mfactor'),...)
	mfactor.list(as.list(x),levels=levels,labels=labels,ordered=ordered,...)

#' @export
mfactor.NULL <- function(x,...)
		return(mfactor.character(x,...))

# a little QC
#-- foo <- data.frame(a=1:2,b=mfactor(c('a','a:b'),':'))
#-- bar <- data.frame(a=3:4,b=mfactor(c('c','c:b'),':'))
#-- foobar <- data.frame(a=1:4,b=mfactor(c('a','a:b','c','c:b'),':'))
#-- stopifnot(identical(rbind(foo,bar),foobar))
#-- rm(foo,bar,foobar)



#-- # used in mfactor-ops.r and mfactor-subset.r
#-- 
#-- .cleanLevels <- function(x){ 
#-- 	# strip the unused mlevels 
#-- 	mlx <- attr(x,'mlevels')
#-- 	if(!length(mlx))
#-- 		return(x)
#-- 	lx <- attr(x, "levels")
#-- 	out <- unclass(x)
#-- 	isMlevel <- out > length(lx)
#-- 	# handle empty mlevels
#-- 	for(i in 1:length(mlx))
#-- 		if(mlx[i] == '')
#-- 			out[ out == (length(lx) + i) & !is.na(out) ] <- 0
#-- 	# handle duplicated mlevels
#-- 	tmp <- c(0:length(lx),length(lx) + match(mlx,mlx))
#-- 	out <- tmp[out+1]
#-- 	# handle un-used levels
#-- 	isUsed <- (length(lx) + 1:length(mlx)) %in% out
#-- 	out <- match(out,
#-- 			   c(0:length(lx),
#-- 				 length(lx) + which(isUsed))) - 1
#-- 	attr(out,'mlevels') <- mlx[isUsed]
#-- 	attr(out, "levels") <- lx
#-- 	class(out) <- class(x)
#-- 	out
#-- }

