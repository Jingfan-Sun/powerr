#' %--% binary operator which add sparse matrix including complex numbers
#' 

"%--%" <- function(x, y) {
    if (.GlobalEnv$Settings$sparse == TRUE) {
        if ((class(x) != 'list') && (class(x) != 'dgCMatrix')) {
            if ((nnzero(Im(x)) != 0)) {
                x <- list(mRe = as(as(powerRe(x), 'matrix'), 'dgCMatrix'),
                          mIm = as(as(powerIm(x), 'matrix'), 'dgCMatrix')); 
            } else {
                x <- as(as.matrix(powerRe(x)), 'dgCMatrix');
            }
        }
        if ((class(y) != 'list') && (class(y) != 'dgCMatrix')) {
            if ((nnzero(Im(y)) != 0)) {
                y <- list(mRe = as(as(powerRe(y), 'matrix'), 'dgCMatrix'),
                          mIm = as(as(powerIm(y), 'matrix'), 'dgCMatrix'));  
            } else {
                y <- as(as.matrix(powerRe(y)), 'dgCMatrix');
            }
        }
        # if they are both sparse matrix of only real number
        if ((class(x) == 'dgCMatrix') && (class(y) == 'dgCMatrix')) {
            temp <- x - y;
        }
        # is one of them is complex and the other is real
        else if ((class(x) == 'list') && (class(y) == 'dgCMatrix')){
            temp <- list(mRe = x$mRe - y,
                         mIm = x$mIm);
        } else if ((class(y) == 'list') && (class(x) == 'dgCMatrix')){
            temp <- list(mRe = x - y$mRe,
                         mIm = y$mIm);
        } 
        # if they are both complex
        else if ((class(x) == 'list') && (class(y) == 'list')) {
            temp <- list(mRe = x$mRe - y$mRe,
                         mIm = x$mIm - y$mIm);
        }
    } else {
        temp <- x - y;
    }
    return(temp);
}