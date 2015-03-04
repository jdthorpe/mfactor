# ------------------------------------------------------------
# A better rbind
# ------------------------------------------------------------

# the native implementations of rbind and rbind.data.frame 
# depend on the implementations on the variables contained
# within data frames (really annoying!)

setMethod('rbind2',
		signature(x='NULL',y='data.frame'),
		function(x,y) y)

#setOldClass('data.frame')

# there is no point in these methods because 
# cbind is not an S3 generic and it will not 
# dispatch on s4 methods when the first argument 
# is not an S4 object and isS4(data.frame(...)) 
# evaluates to False

#-- setMethod('rbind2',
#-- 		signature(x='data.frame',y='NULL'),
#-- 		function(x,y) x)
#-- 
#-- setMethod('rbind2',
#-- 		signature(x='data.frame',y='missing'),
#-- 		function(x,y) x)

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
				compatibilityCheck(x,y,.stop=TRUE)

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


