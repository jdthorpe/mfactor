
#' @export
`length<-.mfactor` <- function (x, value) 
{
    lx <- levels(x)
    cx <- oldClass(x)
    mlx <- attr(x,'mlevels')
    x <- NextMethod()
    structure(x, levels = lx, class = cx, mlevels = mlx)
}

