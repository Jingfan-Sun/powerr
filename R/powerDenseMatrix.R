#' create dense matrix with row and column index
#' 
#' @param rowIndex specify the location of row of non-zero entries in the matrix
#' @param columnIndex specify the location of column of non-zero entries in the matrix
#' @param data non-zero entries on these certain locations
#' @param nrow number of row of the matrix
#' @param ncol number of column of the matrix

powerDenseMatrix <- function(rowIndex, columnIndex, data, nrow, ncol){
    temp <- matrix(0, nrow, ncol);
    index <- apply(cbind(rowIndex, columnIndex), 1, foo <- function(x) {
        x[1] + (x[2] - 1) * nrow
    })
    temp[index] <- data;
    
    return(temp)
}