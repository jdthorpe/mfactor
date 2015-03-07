
#' Special median functions.
#' 
#' When `length(x)`, these special median functions return the upper, lower, or a random 
#' selection from the middle two values of x.  When `length(x)` is odd, each function 
#' is performes identcialy to `base::median`. 
#' 
#' @export lower.median
#' @param x	an object for which a method has been defined, or a numeric vector containing the values whose median is to be computed.
#' @param na.rm	a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @rdname special-median
#' @inheritParams base::median
#' @examples
#' x = c(1,2,3,3,3,4,5,6,7,8)
#' 
#' lower.median(x) # returns 3
#' 
#' upper.median(x) # returns 4
#' 
#' # 3 times more likely to return 3 than 4 b/c of the multiplicity of the value 3 in x
#' median.random(x) 

lower.median=function(x,na.rm=FALSE) {
	if(na.rm){
		x <- x[!is.na(x)]
	}
	if(!length(x) || any(is.na(x)))
		return(NA)
	x[ceiling(length(x)/2)]
}

#' @export upper.median
#' @rdname special-median
#' @inheritParams base::median
upper.median=function(x,na.rm=FALSE) { 
	if(na.rm){
		x <- x[!is.na(x)]
	}
	if(!length(x) || any(is.na(x)))
		return(NA)
	x[floor(length(x)/2)+1]
}

#' A randomly selected median
#' 
#' A randomly selected median.
#' 
#' @export median.random
#' @usage median.random(x,na.rm=FALSE)
#' @rdname special-median
#' @inheritParams base::median
median.random=function(x,na.rm = FALSE) {
	if(na.rm){
		x <- x[!is.na(x)]
	}
	if(!length(x) || any(is.na(x)))
		return(NA)
	U <- upper.median(x)
	L <- lower.median(x)
	c(L,U)[sample.int(2,
					  1,
					  ,
					  c(sum(x == L),sum(x == U)))]
}

