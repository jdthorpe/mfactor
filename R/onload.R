
.onLoad <- function(libname,pkgname)
	# set `rbind()` to use S4 methods of `rbind2`
	methods:::bind_activation(on = TRUE)

