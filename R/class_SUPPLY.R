#' The class SUPPLY 
#'
#' @field n total number of supply

SUPPLY <- setRefClass("SUPPLY", contains = "powerr", 
                      fields = list(n = "numeric"),
                      methods = list(
                          initialize = function(data, n){
                              n <<- numeric();
                              ncol <<- 20;
                          }
                          
                      ))