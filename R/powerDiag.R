#' return Matrix::diag components of either complex or real number sparse matrix
#' TRUE means there is a number at that index, FALSE means not.
#' 

powerDiag <- function(x) {
    if (class(x) != 'list') {
        temp <- Matrix::diag(x);
    } else {
        temp <- apply(cBind(Matrix::diag(x[[1]]), Matrix::diag(x[[2]])), 1, foo <- function(par) {par[1] | par[2]});
    }
    return(temp);
}