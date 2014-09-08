#' find index of non 'num' in a given object
#' 
#' @param input the objext needed to be manipulated
#' @param num the number needed to be avoided in finding

powerFind <- function(input, num) {
    index <- which(input != num, arr.ind = T);
    return(index);
}