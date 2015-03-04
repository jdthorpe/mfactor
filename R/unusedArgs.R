
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

