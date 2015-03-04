
#' @export
#' @method summary mfactor
`summary.mfactor` <- function(x){
	mx <- as.matrix(x)
	rbind('TRUE' = apply(mx,2,sum,na.rm=T),
		  'FALSE'= apply(!mx,2,sum,na.rm=T),
		  '<NA>' = apply(is.na(mx),2,sum))
}

# SUPPORT FOR MIN(),MAX(), ETC: 

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


