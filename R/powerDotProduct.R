#' dot product for either complex or real sparse matrix
#' 

"%.*%" <- function(x, y) {
    if (.GlobalEnv$Settings$sparse == TRUE) {
        if ((class(x) != 'list') && (class(x) != 'dgCMatrix')) {
            if ((nnzero(Im(x)) != 0)) {
                x <- list(mRe = as(Re(x), 'dgCMatrix'),
                          mIm = as(Im(x), 'dgCMatrix')); 
            } else {
                x <- as(as.matrix(Re(x)), 'dgCMatrix');
            }
        }
        if ((class(y) != 'list') && (class(y) != 'dgCMatrix')) {
            if ((nnzero(Im(y)) != 0)) {
                y <- list(mRe = as(Re(y), 'dgCMatrix'),
                          mIm = as(Im(y), 'dgCMatrix')); 
            } else {
                y <- as(as.matrix(Re(y)), 'dgCMatrix');
            }
        }
}