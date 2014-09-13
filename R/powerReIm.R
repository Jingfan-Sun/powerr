#' calculate real part of a complex matrix either sparse or dense
#' 

powerRe <- function(x) {
    if (class(x) == 'list') {
        x <- x[[1]];
    } else if (class(x) == 'dgCMatrix'){
        # do nothing
    } else {
        x <- Re(x);
    }
    return(x);
}

#' calculate imagine part of a complex matrix either sparse or dense
#' 

powerIm <- function(x) {
    if (class(x) == 'list') {
        x <- x[[2]];
    } else if (class(x) == 'dgCMatrix'){
        x <- sparseMatrix(1, 1, x = 0, dims = x@Dim);
    } else {
        x <- Im(x);
    }
    return(x);
}