
#' Test that two (x and y) data.frame's are compatibile for binding via \code{rbind(x,y)}.
#'
#' Identifies a variety of incompatibilities between two \code{data.frame} objects 
#' that could cause errors, or missing data when combined via \code{rbind(x,y)}
#' and raises an error if combining the two data.frames would result in missing 
#' data or other unexpected outcomes.
#'
#' Note that setting \code{'options(strict.Rbind=TRUE)'} will cause this compatibilty check 
#' to be run whenever rbind(x,y) is called with two \code{data.frame} objects.
#'
#' @param x,y \code{data.frame} objects 
#' @export
#' @family mfactor
compatibilityCheck <- function(a,b,verbose=TRUE,.stop=FALSE){
	if(is.null(a)) return(invisible())
	if(is.null(b)) return()

	fields <- names(a)
	out <- data.frame(field=fields,
					  class_a=as.character(NA),
					  class_b=as.character(NA),
					  incompatibility=as.character(NA),
					  stringsAsFactors=FALSE)

	# --------------------------------------------------
	# indexes of factors and mfactors
	# --------------------------------------------------
	fx <- which(sapply(a,inherits,c('factor')))
	fy <- which(sapply(b,inherits,c('factor')))
	mfx <- which(sapply(a,inherits,c('mfactor')))
	mfy <- which(sapply(b,inherits,c('mfactor')))

	# --------------------------------------------------
	# validate factors v. factors
	# --------------------------------------------------
	for(i in intersect(fx,fy)){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(a[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(b[,i]),collapse=';')
		if(!identical(levels(a[,field]),levels(b[,field]))){
			out[out$field == field,'incompatibility'] <- 
				"Factor levels differ between data frames"
		}
	}

	# --------------------------------------------------
	# validate factors v. everything else besides mfactors
	# --------------------------------------------------
	for(i in setdiff(fx,c(mfy,fy))){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(a[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(b[,i]),collapse=';')
		if(inherits(b[,field],c('factor','mfactor'))){
			stop('impossible?')
		} else if(inherits(b[,field],'character')){
			if(!all(b[,field] %in% levels(a[,field]))){
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(b[,"',field,'"],"character") and !all(b[,"',field,'"] %in% levels(a[,"',field,'"]))',sep = '')
		}
		else if(inherits(b[,field],'logical')){
			if(!all(is.na(b[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(b[,"',field,'"],"logical") and !all(is.na(b[,"',field,'"]))',sep = '')
		}
		else if(!all(is.na(b[,field])))
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "a" cannot be coerced to the mfactor in data.frame "b"'
		}
	}

	for(i in setdiff(fy,c(fx,mfx))){
		(field <- fields[i])
		(out[i,'class_a'] <- paste(class(a[,i]),collapse=';'))
		(out[i,'class_b'] <- paste(class(b[,i]),collapse=';'))
		if(inherits(a[,field],c('factor','mfactor'))){
			stop('impossible?')
		} else if(inherits(a[,field],'character')){
			if(!all(a[,field] %in% levels(b[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('!all(a[,"',field,'"] %in% levels(b[,"',field,'"]))',sep = '')
		} else if(inherits(a[,field],'logical')){
			if(!all(is.na(a[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(a[,"',field,'"],"logical") and !all(is.na(a[,"',field,'"]))',sep = '')
		} else if(!all(is.na(a[,field]))) {
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "B" cannot be coerced to the mfactor in data.frame "A"'
		}
	}

	# --------------------------------------------------
	# validate mfactors v. mfactors 
	# --------------------------------------------------
	for(i in intersect(mfx,mfy)){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(a[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(b[,i]),collapse=';')
		levA <- levels(a[,field])
		levB <- levels(b[,field])
		if(length(levA) && length(levB) && !identical(levA,levB)){
			out[out$field == field,'incompatibility'] <- 
				"Factor levels differ between data frames"
		}
	}

	# --------------------------------------------------
	# validate mfactors v. everything else 
	# --------------------------------------------------
	for(i in setdiff(mfx,mfy)){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(a[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(b[,i]),collapse=';')
		if(inherits(b[,field],'factor')){
			if(length(levels(a[,field])) &&
			   !(identical(levels(b[,field]),levels(a[,field]))|
				 identical(levels(b[,field]),c(getOption('mfactor.none','<None>'),levels(a[,field])))))
				out[out$field == field,'incompatibility'] <- "Factor levels differ between data frames"
					#paste('levels of a[,"',field,'"] do not match levels of b[,"',field,'"]',sep = '')
		}
		else if(inherits(b[,field],'character')){
			if(length(levels(a[,field])) &&
			   !all(b[,field] %in% levels(a[,field]))){
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(b[,"',field,'"],"character") and !all(b[,"',field,'"] %in% levels(a[,"',field,'"]))',sep = '')
		}
		else if(inherits(b[,field],'logical')){
			if(!all(is.na(b[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(b[,"',field,'"],"logical") and !all(is.na(b[,"',field,'"]))',sep = '')
		}
		else if(!all(is.na(b[,field])))
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "a" cannot be coerced to the mfactor in data.frame "b"'
		}
	}

	for(i in setdiff(mfy,mfx)){
		(field <- fields[i])
		(out[i,'class_a'] <- paste(class(a[,i]),collapse=';'))
		(out[i,'class_b'] <- paste(class(b[,i]),collapse=';'))
		if(inherits(a[,field],'factor')){
			if(!(identical(levels(a[,field]),levels(b[,field]))|
				 identical(levels(a[,field]),c(getOption('mfactor.none','<None>'),levels(b[,field])))))
				out[out$field == field,'incompatibility'] <- "Factor levels differ between data frames"
					#paste('levels of a[,"',field,'"] do not match levels of b[,"',field,'"]',sep = '')
		} else if(inherits(a[,field],'character')){
			if(!all(a[,field] %in% levels(b[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('!all(a[,"',field,'"] %in% levels(b[,"',field,'"]))',sep = '')
		} else if(inherits(a[,field],'logical')){
			if(!all(is.na(a[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(a[,"',field,'"],"logical") and !all(is.na(a[,"',field,'"]))',sep = '')
		} else if(!all(is.na(a[,field]))) {
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "B" cannot be coerced to the mfactor in data.frame "A"'
		}
	}

	# RAISE AN ERROR IF ANY, OTHERWISE EXIT QUIETLY
	errors <- !is.na(out[,'incompatibility'])
	if(any(errors)){
		if(.stop){
			if(verbose)
				print(out[errors,])
			stop(warningMessage)
		} else{
			warning(warningMessage)
			return(out[errors,])
		}
	}
	# return(NULL)
}

warningMessage <- 'Data frames are not compatible. Set `options(strict.Rbind=FALSE)` to rbind without safety checks.'


