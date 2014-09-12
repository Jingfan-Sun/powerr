#' Create dense or sparse matrix for power system analysis usage
#' 

powerMatrix <- function(rowIndex, columnIndex, x, dims, useSparse = .GlobalEnv$Settings$sparse) {
    if (useSparse) {
        if (class(x) == 'numeric') {
            temp <- sparseMatrix(rowIndex, columnIndex, x = x, dims = dims);
        } else {
            temp <- list(
                mRe = sparseMatrix(rowIndex, columnIndex, x = Re(x), dims = dims),
                mIm = sparseMatrix(rowIndex, columnIndex, x = Im(x), dims = dims)
                )
        }
    } else {
        temp <- powerDenseMatrix(rowIndex, columnIndex, x, dims);
    }
    return(temp);
}