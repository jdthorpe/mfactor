
# used in mfactor-ops.r and mfactor-subset.r

.cleanLevels <- function(x){ 
	# strip the unused mlevels 
	mlx <- attr(x,'mlevels')
	if(!length(mlx))
		return(x)
	lx <- attr(x, "levels")
	out <- unclass(x)
	isMlevel <- out > length(lx)
	# handle empty mlevels
	for(i in 1:length(mlx))
		if(mlx[i] == '')
			out[ out == (length(lx) + i) & !is.na(out) ] <- 0
	# handle duplicated mlevels
	tmp <- c(0:length(lx),length(lx) + match(mlx,mlx))
	out <- tmp[out+1]
	# handle un-used levels
	isUsed <- (length(lx) + 1:length(mlx)) %in% out
	out <- match(out,
			   c(0:length(lx),
				 length(lx) + which(isUsed))) - 1
	attr(out,'mlevels') <- mlx[isUsed]
	attr(out, "levels") <- lx
	class(out) <- class(x)
	out
}

