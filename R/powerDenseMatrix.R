#' powerDenseMatrix
#' 
#' create dense matrix with row and column index
#' 
#' @param rowIndex specify the location of row of non-zero entries in the matrix
#' @param columnIndex specify the location of column of non-zero entries in the matrix
#' @param x non-zero entries on these certain locations
#' @param dims dimention of the matrix
#' 
#' @return dense matrix used in powerr analysis
#' 
#' @examples
#' powerDenseMatrix(1, 1, x = 1, dims = c(3,3))

powerDenseMatrix <- function(rowIndex, columnIndex, x, dims){
    nrow <- dims[1]
    ncol <- dims[2]
    temp <- matrix(0, nrow, ncol);
    
    index <- apply(cbind(rowIndex, columnIndex), 1, function(y) {
        y[[1]] + (y[[2]] - 1) * nrow
    })
    
    apply(cbind(index, as.numeric(1:length(x))), 1, function(y) {
        temp[y[[1]]] <<- temp[y[[1]]] + x[y[[2]]];
        return()
    })
#     temp[index] <- x;
    
    return(temp)
}