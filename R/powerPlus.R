#' %++% binary operator which add sparse matrix including complex numbers
#' 

"%++%" <- function(x, y) {
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
        # if they are both sparse matrix of only real number
        if ((class(x) == 'dgCMatrix') && (class(y) == 'dgCMatrix')) {
            temp <- x + y;
        }
        # is one of them is complex and the other is real
        else if ((class(x) == 'list') && (class(y) == 'dgCMatrix')){
            temp <- list(mRe = x$mRe + y,
                         mIm = x$mIm);
        } else if ((class(y) == 'list') && (class(x) == 'dgCMatrix')){
            temp <- list(mRe = x + y$mRe,
                         mIm = y$mIm);
        } 
        # if they are both complex
        else if ((class(x) == 'list') && (class(y) == 'list')) {
            temp <- list(mRe = x$mRe + y$mRe,
                         mIm = x$mIm + y$mIm);
        }
    } else {
        temp <- x + y;
    }
    return(temp);
}