#' return diag components of either complex or real number sparse matrix
#' TRUE means there is a number at that index, FALSE means not.
#' 

powerDiag <- function(x) {
    if ((class(x) != 'list') && (class(x) != 'dgCMatrix')) {
        x <- as(x, 'dgCMatrix');
    }
    if (class(x) != 'list') {
        temp <- diag(x);
    } else {
        temp <- apply(cbind(diag(x[[1]]), diag(x[[2]])), 1, foo <- function(par) {par[1] | par[2]});
    }
    return(temp);
}

foo <- function(x, y) {
    return(x | y);
}