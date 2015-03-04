
# @export
rep.mfactor <- function (x, ...) {
    y <- NextMethod()
    structure(y, class = class(x), levels = levels(x),mlevels = attr(x,'mlevels'))
}

