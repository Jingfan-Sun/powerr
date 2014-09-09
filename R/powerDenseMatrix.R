#' create dense matrix with row and column index
#' 
#' @param rowIndex specify the location of row of non-zero entries in the matrix
#' @param columnIndex specify the location of column of non-zero entries in the matrix
#' @param x non-zero entries on these certain locations
#' @param dims dimention of the matrix

powerDenseMatrix <- function(rowIndex, columnIndex, x, dims){
    nrow <- dims[1]
    ncol <- dims[2]
    temp <- matrix(0, nrow, ncol);
    index <- apply(cbind(rowIndex, columnIndex), 1, foo <- function(x) {
        x[1] + (x[2] - 1) * nrow
    })
    temp[index] <- x;
    
    return(temp)
}