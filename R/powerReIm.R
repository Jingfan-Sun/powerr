#' powerRe
#' 
#' calculate real part of a complex matrix either sparse or dense
#' 
#' @param x sparse or dense complex matrix
#' 
#' @return real part of the object
#' 
#' @examples
#' # sparse complex 
#' x <- list(sparseMatrix(1, 1, x = 1, dims = c(3,3)), sparseMatrix(2, 2, x = 2, dims = c(3,3)))
#' powerRe(x) # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # dense complex
#' x <- matrix(1+1i, 3, 3)
#' powerRe(x)

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

#' powerIm
#' 
#' calculate imagine part of a complex matrix either sparse or dense
#' 
#' @param x sparse or dense complex matrix
#' 
#' @return imagine part of the object
#' 
#' @examples
#' # sparse complex 
#' x <- list(sparseMatrix(1, 1, x = 1, dims = c(3,3)), sparseMatrix(2, 2, x = 2, dims = c(3,3)))
#' powerIm(x) # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # dense complex
#' x <- matrix(1+1i, 3, 3)
#' powerIm(x)

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