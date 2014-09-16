#' powerDiag
#' 
#' return Matrix::diag components of either complex or real number sparse matrix
#' 
#' @param x complex or real number sparse matrix needed to be calculated diag
#' 
#' @return TRUE means there is a number at that index, FALSE means not.
#' 
#' @examples
#' x <- sparseMatrix(1, 1, x = 1, dims = c(3,3)) # 3 x 3 sparse Matrix of class "dgCMatrix"
#' powerDiag(x) # [1] 1 0 0

powerDiag <- function(x) {
    if (class(x) != 'list') {
        temp <- Matrix::diag(x);
    } else {
        temp <- apply(cBind(Matrix::diag(x[[1]]), Matrix::diag(x[[2]])), 1, foo <- function(par) {par[1] | par[2]});
    }
    return(temp);
}