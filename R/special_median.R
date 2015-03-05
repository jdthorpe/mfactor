
#' Special median functions.
#' 
#' Special median functions, which return the upper(lower) of the two middle values
#' when lenght(x) is even
#' @param a numeric vector containing the values whose (special) median is to be computed
#' @export lower.median
#' @rdname special-median
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
upper.median=function(x,na.rm=FALSE) { 
	if(na.rm){
		x <- x[!is.na(x)]
	}
	if(!length(x) || any(is.na(x)))
		return(NA)
	x[floor(length(x)/2)+1]
}

#' @export median.random
#' @rdname special-median
median.random=function(x,na.rm=FALSE) {
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

