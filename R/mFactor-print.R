
#' @export
print.mfactor <- function(x, 
						  quote = FALSE,
						  max.levels = NULL,
						  width = getOption("width"),
						  ...){
	# MOST OF THIS METHOD WAS COPIED FROM PRINT.FACTOR
	ord <- inherits(x,'ord_mfactor')
	if (length(x) == 0L) 
		cat(if (ord) "ord_mfactor" else "mfactor", "(0)\n", sep = "")
	else 
		print(as.character(x), quote = quote, ...)

	maxl <- if (is.null(max.levels)) TRUE else max.levels
	if (maxl) {
		n <- length(lev <- encodeString(levels(x), quote = ifelse(quote, 
			"\"", "")))
		colsep <- (if (ord) " < " else " ")
		T0 <- "Levels: "
		if (is.logical(maxl)) 
			maxl <- {
				width <- width - (nchar(T0, "w") + 3L + 1L + 
				  3L)
				lenl <- cumsum(nchar(lev, "w") + nchar(colsep, 
				  "w"))
				if (n <= 1L || lenl[n] <= width) 
				  n
				else max(1L, which(lenl > width)[1L] - 1L)
			}
		drop <- n > maxl
		cat(if (drop) 
			paste(format(n), ""), T0, paste(if (drop) 
			c(lev[1L:max(1, maxl - 1)], "...", if (maxl > 1) lev[n])
		else lev, collapse = colsep), "\n", sep = "")
	}
	invisible(x)
}	  

