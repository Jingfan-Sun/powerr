#' Create dense or sparse matrix for power system analysis usage
#' 

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