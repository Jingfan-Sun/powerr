#' return index of the unique number at its first appearence
#' 
#' @param input the object needed to be manipulated

powerUnique <- function(input){
    uniqueIndex <- which(!duplicated(input));
    return(uniqueIndex);
}