
#' A wrapper for utils::write.table() 
#' 
#' A wrapper for utils::write.table() which handles the 
#' coercion of the mfactor variables to character before 
#' writing them to file so that they are quoted appropropriately 
#' @export
write.table <- function(x,...){
    Call <- match.call(expand.dots = TRUE)
	if(inherits(x,'data.frame')){
		attrs <- attributes(x)
		x <- lapply(x,
					function(fld)
						if(inherits(fld,'mfactor')) 
							fld <- as.character(fld)
					   	else 
							fld)
		attributes(x) <- attrs
	}
	Call[['x']] <- x
	Call[1] <- quote(utils::write.table())
    eval.parent(Call)
}


