#' calculate conj of complex or real sparse matrix
#' 

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