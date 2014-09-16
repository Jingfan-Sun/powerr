#' powerConj
#' 
#' Calculate conj of complex or real sparse matrix
#' 
#' @param x complex objects to calculate conjugate

powerConj <- function(x) {
    if (class(x) == 'list') {
        x <- list(mRe = x[[1]],
                  mIm = -x[[2]]);
    } else if (class(x) == 'dgCMatrix'){
        # do nothing
    } else {
        x <- Conj(x);
    }
    return(x);
}