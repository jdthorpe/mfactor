
#' @export
#' @importFrom stats median
#' @method median mfactor
median.mfactor <- function (x, ...) 
	stop(.Generic, " not meaningful for mfactors")
#' @export
Complex.mfactor <- function (x, ...) 
	stop(.Generic, " not meaningful for mfactors")
#' @export
Math.mfactor <- function (x, ...) 
	stop(.Generic, " not meaningful for mfactors")

# inspired by the python function zip
.zip <- function(x,y){
	stopifnot(class(x) == 'list')
	stopifnot(class(y) == 'list')
	stopifnot(length(x) == length(y))
	out <- list()
	for(i in 1:length(x))
		out[[i]] <- list(x[[i]],y[[i]])
	out
}




# returns the levels of x, with missing value replaced by a sring like "(_)+NA"
LEVELS <- function(x) {
	.levels <- levels(x)
	if(is.null(.levels))
		return()
	if (any(IS.NA <- is.na(.levels))) {
		n <- "_NA"
		while (n %in% .levels) 
			n <- paste0("_",n)
		.levels[IS.NA] <- n
	}
	.levels
}


#' @export
#' @method Ops mfactor
Ops.mfactor <- function (e1, e2) {

	# HANDLE UNARY OPERATORS
	if(missing(e2)){
		if(.Generic == '!'){
			out <- as.vector(e1)
			attributes(out) <- attributes(e1)
			return(out)
		}
		warning('the unary operator "',.Generic, '" is not meaningful for multi-factors')
		return(rep.int(NA, length(e1)))
   	}


	# what about NULL values?

	# --------------------------------------------------
	# COERCE ONE OF THE VECORS TO MFACTOR IF NECESSARY
	# --------------------------------------------------
	# FROM THE LANGUAGE DEFINITION: When the group is Ops the special variable 
	# .Method is a string vector with two elements. The elements of .Method are 
	# set to the name of the method if the corresponding argument is a member of 
	# the class that was used to determine the method. Otherwise the corresponding 
	# element of .Method is set to the zero length string, "".
	# --------------------------------------------------
	if (nzchar(.Method[1L])) 
		e2 <- mfactor(e2,levels=LEVELS(e1),ordered = class(e1)[1] == 'ord_mfactor')
	if (nzchar(.Method[2L])) 
		e1 <- mfactor(e1,levels=LEVELS(e2),ordered = class(e2)[1] == 'ord_mfactor')


	if(!identical(levels(e1),levels(e2)))
		stop('Level sets of ',deparse(substitute(e1)),
			 ' and ',deparse(substitute(e2)),
			 'are different')

	# only 'wrap' when the shorter vector has length 1
	if(length(e1)!=length(e1)){
		if(!(length(e1) == 1 || length(e2) == 1))
			stop('shorter object length is not equal to 1')
		if(length(e1) == 1)
			e1 <- rep_len(e1, length(e2))
		else
			e2 <- rep_len(e2, length(e1))
	} 

	nas <- is.na(e1) | is.na(e2)
	if(.Generic == '+'){

		out <- as.vector(e1) | as.vector(e2)
		attibures(out)  <-  attributes(e1)
		return(out)

	} else if(.Generic == '-' ){

		out <- as.vector(e1) & !as.vector(e2)
		attibures(out)  <-  attributes(e1)
		return(out)

	} else if(.Generic == `==`){
		if(getOption('mfactor.strict.compare',TRUE))
			return(apply(as.matrix(e1) == as.matrix(e2),
						 1,
						 all))
		else
			return(apply(as.matrix(e1) | as.matrix(e2),
						 1,
						 any))
	}else if(.Generic == `!=`){
		if(getOption('mfactor.strict.compare',TRUE))
			return(!apply(as.matrix(e1) == as.matrix(e2),
						 1,
						 all))
		else
			return(!apply(as.matrix(e1) | as.matrix(e2),
						 1,
						 any))
		
	} else {
		warning(.Generic, " is not meaningful for multi-factors")
		return(rep.int(NA, length(e1)))
	}
		
	
}


#-- Ops.mfactor <- function (e1, e2) 
#-- {
#-- 	strictMode <- getOption('mfactor.strict.compare',TRUE)
#-- 	# handle empty vectors without 
#-- 	if(!length(levels(e1)) & length(levels(e2)))
#-- 	if(!length(levels(e1)) & length(levels(e2)))
#-- 	if(strictMode & !identical(levels(e1),levels(e2)))
#-- 		stop('Level sets of m-factors are different. Set `options(mfactor.strict.compare=FALSE)` for a loose equality test.')
#--	 ok <- switch(.Generic, '-'= ,'+'= ,`==`= ,`!=` = TRUE, FALSE)
#--	 if (!ok) {
#--		 warning(.Generic, " not meaningful for multi-factors")
#--		 return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
#--	 }
#-- 	.zip <- function(x,y){
#-- 		stopifnot(class(x) == 'list')
#-- 		stopifnot(class(y) == 'list')
#-- 		if(!(length(x) == length(y)))
#-- 		stopifnot(length(x) == length(y))
#-- 		out <- list()
#-- 		for(i in 1:length(x))
#-- 			out[[i]] <- list(x[[i]],y[[i]])
#-- 		out
#-- 	}
#--	 noNA.levels <- function(f) {
#--		 r <- levels(f)
#--		 if (any(ina <- is.na(r))) {
#--			 n <- "  NA "
#--			 while (n %in% r) n <- paste(n, ".")
#--			 r[ina] <- n
#--		 }
#--		 r
#--	 }
#-- 	ce1 <- class(e1)
#--	 if (nzchar(.Method[1L])) {
#--		 l1 <- noNA.levels(e1)
#-- 		e2 <- mfactor(e2,levels=l1)
#-- 	}
#--	 if (nzchar(.Method[2L])) {
#--		 l2 <- noNA.levels(e2)
#-- 		e1 <- mfactor(e1,levels=l2)
#-- 	}
#--	 if (all(nzchar(.Method)) && (length(l1) != length(l2) || 
#--		 !all(sort.int(l2) == sort.int(l1)))) 
#--		 stop("level sets of factors are different")
#-- 	# usual R wrapping logic 
#-- 	if ((s <- length(e1)) < (m <- length(e2))){ 
#-- 		if(m%%s)
#-- 			warning('longer object length is not a multiple of shorter object length')
#-- 		e1 <- rep(e1, length.out = m)
#-- 	} else if ((s <- length(e2)) < (m <- length(e1))) {
#-- 		if(m%%s)
#-- 			warning('longer object length is not a multiple of shorter object length')
#-- 		e2 <- rep(e2, length.out = m)
#-- 	}
#--	 nas <- is.na(e1) | is.na(e2)
#-- 	func <- switch(.Generic, 
#-- 				   `+` = function(x) union(x[[1]], x[[2]]),
#-- 				   `-` = function(x) setdiff(x[[1]], x[[2]]),
#-- 				   `==` = if(strictMode)
#-- 							   function(x) identical(x[[1]],x[[2]])
#-- 						   else 
#-- 							   function(x) any(x[[1]] %in% x[[2]]),
#-- 				   `!=` = if(strictMode)
#-- 							   function(x) !identical(x[[1]],x[[2]])
#-- 						   else 
#-- 							   function(x) !any(x[[1]] %in% x[[2]]))
#--	 if(.Generic %in% c('-' ,'+')){
#-- 		value <- lapply(.zip(as.list.mfactor(e1,TRUE),as.list.mfactor(e2,TRUE)),func)
#-- 		value[nas] <- NA
#-- 		return(.cleanLevels(mfactor(value,
#-- 				levels=1:length(l1),
#-- 				labels = l1,
#-- 				ordered='ord_mfactor' %in% ce1)))#wlog
#-- 	} else{#(.Generic %in% c('==' ,'!='))
#-- 		value <- sapply(.zip(as.list.mfactor(e1,TRUE),as.list.mfactor(e2,TRUE)),func)
#-- 		value[nas] <- NA
#-- 		return(.cleanLevels(value))
#-- 	}
#-- }


notMeaningful  <-  function(e1,e2,.Generic){

		class1 <- class(e1)
		if(identical(class1,c('ord_mfactor','mfactor')))
			class1 <- 'Ordered Multi-factors'
		else if(identical(class1,'mfactor'))
			class1 <- 'Multi-factors'
		else 
			class1 <- sprintf('objects of class %s',
							  paste('"',class1,'"',sep = '',collapse = ','))

		class2 <- class(e2)
		if(identical(class2,c('ord_mfactor','mfactor')))
			class2 <- 'Ordered Multi-factors'
		else if(identical(class2,'mfactor'))
			class2 <- 'Multi-factors'
		else 
			class2 <- sprintf('objects of class %s',
							  paste('"',class2,'"',sep = '',collapse = ','))

		class2 <- class(e2)
		sprintf("'%s' is not meaningful for %s and %s",.Generic,class1,class2)
}

# THE MAXIMUM INDEX IN EACH ROW OF A LOGICAL MATRIX
.MAX<- function(mx){
	# use which.max to find (ncol() - largest index - 1 )
	(indx <- apply(mx[,seq(ncol(mx),1)], 1, which.max))
	(indx <- sapply(indx,function(x)if(length(x)) x else NA))
	(indx <- ncol(mx)-indx + 1)
	# quirk: which.max returns ingeger(0) for nas
	indx[!apply(mx,1,any) & !is.na(indx)] <- 0
	indx
}


#' @export
#' @method Ops ord_mfactor
Ops.ord_mfactor <- function (e1, e2) {
	
	if(.Generic %in%  c('-','+','==','!=','!'))
		return(NextMethod(e1, e2))

	# HANDLE THE CASE WHERE ONE OF THE OBJECTS IS NOT AN ORDERED MFACTOR
	if(!all(nzchar(.Method))){
		# at least one non-mFactor
		if(!nzchar(.Method[1])){
			if(inherits(e1,c('mfactor','factor'))){
				e1 <- as.mfactor(e1,ordered=T)
			}else if(inherits(e1,c('character'))){
				e1 <- as.mfactor(e1,levels=levels(e2),ordered=T)
			}else {
				warning(notMeaningful(e1,e2,.Generic))
				return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
			}
		} else {
			# if(!nzchar(.Method[1]))
			if(inherits(e2,c('mfactor','factor'))){
				e2 <- as.mfactor(e2,ordered=T)
			}else if(inherits(e2,c('character'))){
				e2 <- as.mfactor(e2,levels=levels(e1),ordered=T)
			}else {
				warning(notMeaningful(e1,e2,.Generic))
				return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
			}
		}
	}

	if(!identical(levels(e1),levels(e2)))
		stop('level sets of mfactors are different')

#-- 	mx <-matrix(c(T,F,F,
#-- 				  F,T,F,
#-- 				  F,F,T,
#-- 				  F,F,F,
#-- 				  T,F,T,
#-- 				  NA,NA,NA),ncol=3,byrow=T)

	if(.Generic==`>` )
		return( .MAX(as.matrix(e1)) > .MAX(as.matrix(e2)))
	else if(.Generic==`>=`)
		return(out = .MAX(as.matrix(e1)) >= .MAX(as.matrix(e2)))
	else if(.Generic==`<` )
		return(out = .MAX(as.matrix(e1)) < .MAX(as.matrix(e2)))
	else if(.Generic==`<=`)
		return(out = .MAX(as.matrix(e1)) <= .MAX(as.matrix(e2)))
	else{
		warning(sprintf("'%s' is not meaningful for ordered multi-factors", .Generic))
		return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
	}


}

