#' %**% binary operator which calculate production of sparse matrix including complex numbers

"%**%" <- function(x, y) {
    if ((class(x) != 'list') && (class(x) != 'dgCMatrix')) {
        x <- as(x, 'dgCMatrix');
    }
    if ((class(y) != 'list') && (class(y) != 'dgCMatrix')) {
        y <- as(y, 'dgCMatrix');
    }
    # if they are both sparse matrix of only real number
    if ((class(x) == 'dgCMatrix') && (class(y) == 'dgCMatrix')) {
        temp <- x %*% y;
    }
    # is one of them is complex and the other is real
    else if ((class(x) == 'list') && (class(y) == 'dgCMatrix')){
        result <- powerSparseProduct(x$mRe, x$mIm, y, as(Matrix(0,dim(y)[1], dim(y)[2]), "dgCMatrix"));
        if (nnzero(result$mIm) == 0) {
            temp <- result$mRe;
        } else {
            temp <- list(mRe = result$mRe,
                         mIm = result$mIm);   
        }
    } else if ((class(y) == 'list') && (class(x) == 'dgCMatrix')){
        result <- powerSparseProduct(x, as(Matrix(0,dim(x)[1], dim(x)[2]), "dgCMatrix"), y$mRe, y$mIm);
        if (nnzero(result$mIm) == 0) {
            temp <- result$mRe;
        } else {
            temp <- list(mRe = result$mRe,
                         mIm = result$mIm);   
        }
    } 
    # if they are both complex
    else if ((class(x) == 'list') && (class(y) == 'list')) {
        result <- powerSparseProduct(x$mRe, x$mIm, y$mRe, y$mIm);
        if (nnzero(result$mIm) == 0) {
            temp <- result$mRe;
        } else {
            temp <- list(mRe = result$mRe,
                         mIm = result$mIm);   
        }
    }
    return(temp);
}