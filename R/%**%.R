"%**%" <- function(x, y) {
    # if they are both aprse matrix of only real number
    if ((class(x) == 'dgCMatrix') && (class(y) == 'dgCMatrix')) {
        temp <- x %*% y;
    } else if ((class(x) == 'list') && (class(y) == 'dgCMatrix')){
        
    }
}