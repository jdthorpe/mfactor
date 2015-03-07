
#' Test that two (x and y) data.frame's are compatibile for binding via \code{rbind(x,y)}.
#'
#' Identifies x variety of incompatibilities between two \code{data.frame} objects 
#' that could cause errors, or missing data when combined via \code{rbind(x,y)}
#' and raises an error if combining the two data.frames would result in missing 
#' data or other unexpected outcomes.
#'
#' Note that setting \code{'options(strict.Rbind=TRUE)'} will cause this compatibilty check 
#' to be run whenever rbind(x,y) is called with two \code{data.frame} objects.
#'
#' @param x,y \code{data.frame} objects 
#' @param verbose If TRUE, and errors are present, the table of errors is printed to the console.
#' @param .stop If TRUE, and errors are present, execution is stopped via `stop()`.
#' @export
#' @family mfactor
compatibilityCheck <- function(x,y,verbose=TRUE,.stop=FALSE){
	if(is.null(x)) return(invisible())
	if(is.null(y)) return()

	fields <- names(x)
	out <- data.frame(field=fields,
					  class_a=as.character(NA),
					  class_b=as.character(NA),
					  incompatibility=as.character(NA),
					  stringsAsFactors=FALSE)

	# --------------------------------------------------
	# indexes of factors and mfactors
	# --------------------------------------------------
	fx <- which(sapply(x,inherits,c('factor')))
	fy <- which(sapply(y,inherits,c('factor')))
	mfx <- which(sapply(x,inherits,c('mfactor')))
	mfy <- which(sapply(y,inherits,c('mfactor')))

	# --------------------------------------------------
	# validate factors v. factors
	# --------------------------------------------------
	for(i in intersect(fx,fy)){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(x[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(y[,i]),collapse=';')
		if(!identical(levels(x[,field]),levels(y[,field]))){
			out[out$field == field,'incompatibility'] <- 
				"Factor levels differ between data frames"
		}
	}

	# --------------------------------------------------
	# validate factors v. everything else besides mfactors
	# --------------------------------------------------
	for(i in setdiff(fx,c(mfy,fy))){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(x[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(y[,i]),collapse=';')
		if(inherits(y[,field],c('factor','mfactor'))){
			stop('impossible?')
		} else if(inherits(y[,field],'character')){
			if(!all(y[,field] %in% levels(x[,field]))){
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(y[,"',field,'"],"character") and !all(y[,"',field,'"] %in% levels(x[,"',field,'"]))',sep = '')
		}
		else if(inherits(y[,field],'logical')){
			if(!all(is.na(y[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(y[,"',field,'"],"logical") and !all(is.na(y[,"',field,'"]))',sep = '')
		}
		else if(!all(is.na(y[,field])))
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "x" cannot be coerced to the mfactor in data.frame "y"'
		}
	}

	for(i in setdiff(fy,c(fx,mfx))){
		(field <- fields[i])
		(out[i,'class_a'] <- paste(class(x[,i]),collapse=';'))
		(out[i,'class_b'] <- paste(class(y[,i]),collapse=';'))
		if(inherits(x[,field],c('factor','mfactor'))){
			stop('impossible?')
		} else if(inherits(x[,field],'character')){
			if(!all(x[,field] %in% levels(y[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('!all(x[,"',field,'"] %in% levels(y[,"',field,'"]))',sep = '')
		} else if(inherits(x[,field],'logical')){
			if(!all(is.na(x[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(x[,"',field,'"],"logical") and !all(is.na(x[,"',field,'"]))',sep = '')
		} else if(!all(is.na(x[,field]))) {
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "B" cannot be coerced to the mfactor in data.frame "A"'
		}
	}

	# --------------------------------------------------
	# validate mfactors v. mfactors 
	# --------------------------------------------------
	for(i in intersect(mfx,mfy)){
		(field <- fields[i])
		out[i,'class_a'] <- paste(class(x[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(y[,i]),collapse=';')
		levA <- levels(x[,field])
		levB <- levels(y[,field])
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
		out[i,'class_a'] <- paste(class(x[,i]),collapse=';')
		out[i,'class_b'] <- paste(class(y[,i]),collapse=';')
		if(inherits(y[,field],'factor')){
			if(length(levels(x[,field])) &&
			   !(identical(levels(y[,field]),levels(x[,field]))|
				 identical(levels(y[,field]),c(getOption('mfactor.none','<None>'),levels(x[,field])))))
				out[out$field == field,'incompatibility'] <- "Factor levels differ between data frames"
					#paste('levels of x[,"',field,'"] do not match levels of y[,"',field,'"]',sep = '')
		}
		else if(inherits(y[,field],'character')){
			if(length(levels(x[,field])) &&
			   !all(y[,field] %in% levels(x[,field]))){
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(y[,"',field,'"],"character") and !all(y[,"',field,'"] %in% levels(x[,"',field,'"]))',sep = '')
		}
		else if(inherits(y[,field],'logical')){
			if(!all(is.na(y[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(y[,"',field,'"],"logical") and !all(is.na(y[,"',field,'"]))',sep = '')
		}
		else if(!all(is.na(y[,field])))
			out[out$field == field,'incompatibility'] <- 
				'field in dataframe "x" cannot be coerced to the mfactor in data.frame "y"'
		}
	}

	for(i in setdiff(mfy,mfx)){
		(field <- fields[i])
		(out[i,'class_a'] <- paste(class(x[,i]),collapse=';'))
		(out[i,'class_b'] <- paste(class(y[,i]),collapse=';'))
		if(inherits(x[,field],'factor')){
			if(!(identical(levels(x[,field]),levels(y[,field]))|
				 identical(levels(x[,field]),c(getOption('mfactor.none','<None>'),levels(y[,field])))))
				out[out$field == field,'incompatibility'] <- "Factor levels differ between data frames"
					#paste('levels of x[,"',field,'"] do not match levels of y[,"',field,'"]',sep = '')
		} else if(inherits(x[,field],'character')){
			if(!all(x[,field] %in% levels(y[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('!all(x[,"',field,'"] %in% levels(y[,"',field,'"]))',sep = '')
		} else if(inherits(x[,field],'logical')){
			if(!all(is.na(x[,field])))
				out[out$field == field,'incompatibility'] <- 
					paste('inherits(x[,"',field,'"],"logical") and !all(is.na(x[,"',field,'"]))',sep = '')
		} else if(!all(is.na(x[,field]))) {
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


