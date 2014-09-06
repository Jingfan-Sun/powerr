#' The class SUPPLY 
#'
#' @field data supply data
#' @field n total number of supply

SUPPLY <- setRefClass("SUPPLY", 
                      fields = list(data = "matrix",
                                    n = "numeric"),
                      methods = list(
                          initialize = function(data, n){
                              data <<- matrix();
                              n <<- numeric()
                          }
                          
                      ))