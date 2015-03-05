
#' @export
#' @importFrom stats median
#' @method median mfactor
median.mfactor <- function (x, ...) 
    stop(.Generic, " not meaningful for mfactors")

#' @export
Complex.mfactor <- function (z) 
    stop(.Generic, " not meaningful for mfactors")

#' @export
Math.mfactor <- function (x, ...) 
    stop(.Generic, " not meaningful for mfactors")

#' @export
#' @method Ops mfactor
Ops.mfactor <- function (e1, e2) 
{
	strictMode <- getOption('mfactor.strict.compare',TRUE)
	# handle empty vectors without 
	if(!length(levels(e1)) & length(levels(e2)))
	if(!length(levels(e1)) & length(levels(e2)))
	if(strictMode & !identical(levels(e1),levels(e2)))
		stop('Level sets of m-factors are different. Set `options(mfactor.strict.compare=FALSE)` for a loose equality test.')
    ok <- switch(.Generic, '-'= ,'+'= ,`==`= ,`!=` = TRUE, FALSE)
    if (!ok) {
        warning(.Generic, " not meaningful for multi-factors")
        return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
    }
	zip <- function(x,y){
		stopifnot(class(x) == 'list')
		stopifnot(class(y) == 'list')
		if(!(length(x) == length(y)))
		stopifnot(length(x) == length(y))
		out <- list()
		for(i in 1:length(x))
			out[[i]] <- list(x[[i]],y[[i]])
		out
	}
    noNA.levels <- function(f) {
        r <- levels(f)
        if (any(ina <- is.na(r))) {
            n <- "  NA "
            while (n %in% r) n <- paste(n, ".")
            r[ina] <- n
        }
        r
    }
	ce1 <- class(e1)
    if (nzchar(.Method[1L])) {
        l1 <- noNA.levels(e1)
		e2 <- mfactor(e2,levels=l1)
	}
    if (nzchar(.Method[2L])) {
        l2 <- noNA.levels(e2)
		e1 <- mfactor(e1,levels=l2)
	}
    if (all(nzchar(.Method)) && (length(l1) != length(l2) || 
        !all(sort.int(l2) == sort.int(l1)))) 
        stop("level sets of factors are different")
	# usual R wrapping logic 
	if ((s <- length(e1)) < (m <- length(e2))){ 
		if(m%%s)
			warning('longer object length is not a multiple of shorter object length')
		e1 <- rep(e1, length.out = m)
	} else if ((s <- length(e2)) < (m <- length(e1))) {
		if(m%%s)
			warning('longer object length is not a multiple of shorter object length')
		e2 <- rep(e2, length.out = m)
	}
    nas <- is.na(e1) | is.na(e2)
	func <- switch(.Generic, 
				   `+` = function(x) union(x[[1]], x[[2]]),
				   `-` = function(x) setdiff(x[[1]], x[[2]]),
				   `==` = if(strictMode)
							   function(x) identical(x[[1]],x[[2]])
						   else 
							   function(x) any(x[[1]] %in% x[[2]]),
				   `!=` = if(strictMode)
							   function(x) !identical(x[[1]],x[[2]])
						   else 
							   function(x) !any(x[[1]] %in% x[[2]]))
    if(.Generic %in% c('-' ,'+')){
		value <- lapply(zip(as.list.mfactor(e1,TRUE),as.list.mfactor(e2,TRUE)),func)
		value[nas] <- NA
		return(.cleanLevels(mfactor(value,
				levels=1:length(l1),
				labels = l1,
				ordered='ord_mfactor' %in% ce1)))#wlog
	} else{#(.Generic %in% c('==' ,'!='))
		value <- sapply(zip(as.list.mfactor(e1,TRUE),as.list.mfactor(e2,TRUE)),func)
		value[nas] <- NA
		return(.cleanLevels(value))
	}
}

#' @export
#' @method Ops ord_mfactor
Ops.ord_mfactor <- function (e1, e2) {
    ok <- switch(.Generic, '-'= ,'+'= , `<` = , `>` = , `<=` = , `>=` = , 
        `==` = , `!=` = TRUE, FALSE)
    if (!ok) {
        warning(sprintf("'%s' is not meaningful for ordered multi-factors", 
            .Generic))
        return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
    }
    if (.Generic %in% c("==", "!=","+","-")) 
        return(NextMethod(.Generic))
    nas <- is.na(e1) | is.na(e2) | (unclass(e1)==0) | (unclass(e2)==0)
    ord1 <- FALSE
    ord2 <- FALSE
    if (nzchar(.Method[1L])) {
        l1 <- levels(e1)
        ord1 <- TRUE
    } else {
        e1 <- match(e1, levels(e2))
	}
    if (nzchar(.Method[2L])) {
        l2 <- levels(e2)
        ord2 <- TRUE
    } else {
        e2 <- match(e2,levels(e1))
	}
    if (all(nzchar(.Method)) && (length(l1) != length(l2) || 
        !all(l2 == l1))) 
        stop("level sets of factors are different")
	value <- switch(.Generic, 
				   `>`  = function(e1,e2) as.integer(min(e1)) >  as.integer(max(e2)),
				   `>=` = function(e1,e2) as.integer(min(e1)) >= as.integer(max(e2)),
				   `<`  = function(e1,e2) as.integer(max(e1)) <  as.integer(min(e2)),
				   `<=` = function(e1,e2) as.integer(max(e1)) <= as.integer(min(e2))
				   )(e1, e2)
    value[nas] <- NA
    value
}

