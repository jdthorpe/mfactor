# ------------------------------------------------------------
# A better rbind
# ------------------------------------------------------------

# the native implementations of rbind and rbind.data.frame 
# depend on the implementations on the variables contained
# within data frames (really annoying!)

setMethod('rbind2',
		signature(x='data.frame',y='data.frame'),
		function(x,y){ 
			# based on inner function 'match.names' from base:::rbind.data.frame
			test.names <- function(clabs, nmi) {
				if (identical(clabs, nmi)) 
					return()
				if (length(nmi) == length(clabs) && all(nmi %in% clabs)) {
					if (any(pmatch(nmi, clabs, 0L) == 0L)) 
						stop("names do not match previous names")
					return()
				}
				stop("In Rbind(), names of data.frames do not match")
			}
			# rbind fails when a field in one dataset contains factors and the others does not. 
			# so find the factors, and convert the equivelent fields in the other dataset 
			# to factors, and vice versa.

			# note that when calling browse() in this function, 
			# you'll want to skip listing the calls y/c they 
			# print out the entire data frames as in: 
			# browser(skipCalls=500)

			test.names(names(x),names(y))

			# ------------------------------
			# HANDLE THE mFACTOR VARIABLES
			# ------------------------------

			mfx <- sapply(x,function(x)inherits(x,'mfactor'))
			mfy <- sapply(y,function(x)inherits(x,'mfactor'))
			(mfx <- names(mfx[mfx]))
			(mfy <- names(mfy[mfy]))

			# fast path 
			if(!(length(mfx)|length(mfy)))
				return(callNextMethod())

			strict <- getOption('strict.Rbind',FALSE)
			if(strict)
				compatibilityCheck(x,y)

			for(field in setdiff(mfx,mfy))
				y[,field] <- mfactor(y[,field],levels=levels(x[,field]))

			for(field in setdiff(mfy,mfx))
				x[,field] <- mfactor(x[,field],levels=levels(y[,field]))

			# ALLIGN THE ATTRIBUTES OF THE MFACTOR FIELDS
			mfxy <- union(mfx,mfy)
			ord <- logical(length(mfxy))
			names(ord) <- mfxy
			for(field in mfxy){
				# HANDLE FIELDS WHERE THE LEVELS
				if(!length(levels(y[,field])) & !length(levels(x[,field])))
					next
				if(!length(levels(x[,field])))
					attributes(x[,field]) <- attributes(y[,field])
				if(!length(levels(y[,field])))
					attributes(y[,field]) <- attributes(x[,field])
				ord[field] <- (inherits(x[,field],'ord_mfactor') & inherits(y[,field],'ord_mfactor'))
				if(!strict)
					if(!identical(levels(x[,field]),
								  levels(y[,field]))){
						ord[field] <- FALSE # no guarantee that the order makes sense
						lvls <- unique(c(levels(x[,field]),levels(y[,field])))
						x[,field] <- mfactor(x[,field],levels=lvls)
						y[,field] <- mfactor(y[,field],levels=lvls)
					}
				tmp <- mfactor.matrix(rbind(as.matrix(x[,field]), as.matrix(y[,field])), ordered=ord[field])
				indx <- 1:length(x[,field])
				x[,field] <- tmp[indx]
				y[,field] <- tmp[-indx]
				rm(tmp)
			}

			metadata <- list()
			for(field in mfxy){
				#CAPTURE THE METAT DATA FOR THE MFACTOR FIELDS
				metadata[[field]] <- attributes(x[,field])
				# unclass the fields
				x[,field] <- unclass(x[,field])
				y[,field] <- unclass(y[,field])
				attributes(x[,field]) <- NULL
				attributes(y[,field]) <- NULL
			}

			warned <- FALSE
			withCallingHandlers({
				out <- base:::rbind(x,y)
			},warning=function(e){
				if(e$message == "invalid factor level, NAs generated")
					warned <<-TRUE
				else
					warning(e$message,call.=FALSE)
				invokeRestart('muffleWarning')
			})

			#RECLASS AND ATTRIBUTE THE FIELDS
			for(field in mfxy){
				attributes(out[,field]) <- metadata[[field]]
				tryCatch(class(out[,field]) <- c(if(ord[field])'ord_mfactor','mfactor'),error=function(err)browser())
			}

			if(warned)
				for(name in names(x)){
					withCallingHandlers(rbind(data.frame(foo = x[,name]),
											  data.frame(foo = y[,name])),
										warning=function(e){
											if(e$message == "invalid factor level, NAs generated")
												cat('invalid factor level, NAs generated when merging field: "',name,'"',
													', class x = "',class(x[,name]),'"',
													', class y = "',class(y[,name]),'"',
													'\n',sep = '')
											
											warning(paste('invalid factor level, NAs generated when merging field: "',name,'"',sep = ''),
													call.=FALSE)
										})
				}
			return(out)
		})



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
compatibilityCheck <- function(a,b){
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
	if(!any(errors))
		return(invisible(out))
	print(out[errors,])
	stop('Data frames are not compatible. Set `options(strict.Rbind=FALSE)` to rbind without safety checks.')
}

# ------------------------------------------------
# a little utility function
# ------------------------------------------------
# returns the system warning message for unused 
# arguments for any supplied arguents
.unusedArgMessage <- function(...,ValidArgNames){
	dots <- list(...)
	if(!missing(ValidArgNames))
		for(name in ValidArgNames)
			dots[name] <- NULL
	err <- NULL
	store <- function(err)
		err <<-err
	withRestarts(
		 #tryCatch((function(){})(...),
		 tryCatch(do.call(function(){},dots),
				  error=function(err){
					  store(err$message)
				  }))
	return(err)
}



