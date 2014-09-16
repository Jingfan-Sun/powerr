#' Binary operator \%.*\%
#' 
#' Binary operator used to calculate dot product for either complex or real sparse matrix
#' When is not in sparse mode (Settings$sparse == FALSE), '\%.*\%' is same as '*'
#' WHen in sparse mode, '\%.*\%'support complex sparse matrix created by powerMatrix
#' 
#' @param x,y parameters on the other side of the operator
#' 
#' @examples
#' \donttest{
#' # in sparse mode
#' a <- powerMatrix(2, 1, x = 1, dims = c(3,3), useSparse = TRUE)
#' b <- powerMatrix(c(2,3), c(1,2), x = c(1-1i, 3+4i), dims = c(3,3), useSparse = TRUE)
#' a %.*% b
#' # $mRe
#' # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # 
#' # [1,] . . .
#' # [2,] 1 . .
#' # [3,] . . .
#' # 
#' # $mIm
#' # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # 
#' # [1,]  . . .
#' # [2,] -1 . .
#' # [3,]  . . .
#' }

"%.*%" <- function(x, y) {
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
            temp <- x * y;
        }
        # is one of them is complex and the other is real
        else if ((class(x) == 'list') && (class(y) == 'dgCMatrix')){
            temp <- list(mRe = x$mRe * y,
                         mIm = x$mIm * y);   
        } else if ((class(y) == 'list') && (class(x) == 'dgCMatrix')){
            temp <- list(mRe = y$mRe * x,
                         mIm = y$mIm * x);   
        } 
        # if they are both complex
        else if ((class(x) == 'list') && (class(y) == 'list')) {
            temp <- list(mRe = y$mRe * x$mRe - y$mIm * x$mIm,
                         mIm = y$mIm * x$mRe + y$mRe * x$mIm);  
        }
    } else {
        temp <- x * y;
    }
    return(temp);
}