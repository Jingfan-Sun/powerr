#' %--% binary operator which add sparse matrix including complex numbers
#' 

"%--%" <- function(x, y) {
    if ((class(x) != 'list') && (class(x) != 'dgCMatrix')) {
        x <- as(x, 'dgCMatrix');
    }
    if ((class(y) != 'list') && (class(y) != 'dgCMatrix')) {
        y <- as(y, 'dgCMatrix');
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
        remp <- list(mRe = x$mRe - y$mRe,
                     mIm = x$mIm - y$mIm);
    }
    return(temp);
}