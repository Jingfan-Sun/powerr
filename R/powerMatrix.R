#' powerMatrix
#' 
#' Create dense or sparse matrix for power system analysis usage
#' 
#' @param rowIndex row index of nonzeros
#' @param columnIndex column index of nonzeros
#' @param x value in corresponding nonzero index
#' @param dims dimension of the sparse matrix to be created
#' @param useSparse global variable which controls sparse or not
#' 
#' @return matrix used in powerr analysis
#' 
#' @examples
#' # don't use sparse
#' powerMatrix(1, 1, x = 1, dims = c(3,3), useSparse = FALSE)
#' # use sparse
#' # real matrix
#' powerMatrix(1, 1, x = 1, dims = c(3,3), useSparse = TRUE) # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # complex matrix
#' powerMatrix(1, 1, x = 1 + 1i, dims = c(3,3), useSparse = TRUE)
#' # $mRe
#' # 3 x 3 sparse Matrix of class "dgCMatrix"
#' # $mIm
#' # 3 x 3 sparse Matrix of class "dgCMatrix"

powerMatrix <- function(rowIndex, columnIndex, x, dims, useSparse = .GlobalEnv$Settings$sparse) {
    if ((length(rowIndex) == 1) && (length(columnIndex) > 1)) {
        rowIndex <- rep(rowIndex, length(columnIndex));
    } else if ((length(rowIndex) > 1) && (length(columnIndex) == 1)) {
        columnIndex <- rep(columnIndex, length(rowIndex));
    }
    if (useSparse) {
        if (class(x) == 'numeric') {
            temp <- sparseMatrix(rowIndex, columnIndex, x = x, dims = dims);
        } else if (class(x) == 'list') {
            temp <- list(
                mRe = sparseMatrix(rowIndex, columnIndex, x = as.vector(x$mRe), dims = dims),
                mIm = sparseMatrix(rowIndex, columnIndex, x = as.vector(x$mIm), dims = dims)
            )
        } else {
            temp <- list(
                mRe = sparseMatrix(rowIndex, columnIndex, x = as.vector(powerRe(x)), dims = dims),
                mIm = sparseMatrix(rowIndex, columnIndex, x = as.vector(powerIm(x)), dims = dims)
            )
        }
    } else {
        temp <- powerDenseMatrix(rowIndex, columnIndex, x, dims);
    }
    return(temp);
}