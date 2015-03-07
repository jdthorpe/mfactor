
# description is attached to matrix.mfactor
#' @export
#' @rdname mfactor-matrix
#' @family Coercion-from-mfactor
as.matrix.mfactor <- function(x,...){
	# convert to a matrix of indicator variables
	ux <- unclass(x)
	lx <- levels(x)
	if(!length(lx))
		stop('matrix with zero colums')
	nl <- length(lx)
	nx <- names(x)
	mlx <- attributes(x)$mlevels
	nml <- length(mlx)
	out <- matrix(FALSE,length(x),nl,dimnames = list(nx,lx))
	out[is.na(ux),] <- NA
	for(i in 1:nl)
		out[ux == i,i] <- TRUE
	if(nml){
		for(i in 1:nml)
			out[ux == i+nl,as.numeric(strsplit(mlx[i],',')[[1]])] <- TRUE
	}
	out
}

#-- as.matrix.mfactor <- function(x){
#-- 	# convert to a matrix of indicator variables
#-- 	out <- NULL
#-- 	lx <- levels(x)
#-- 	nl <- length(lx)
#-- 	nx <- names(x)
#-- 	listx <- as.list.mfactor(x,TRUE)
#-- 	out <- t(matrix(unlist(lapply(listx,function(x)1:nl %in% x)),nl))
#-- 	dimnames(out) <- list(nx,lx)
#-- 	out
#-- }



