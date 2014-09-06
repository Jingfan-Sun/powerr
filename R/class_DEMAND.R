#' The class DEMAND 
#'
#' @field data demand load data
#' @field n total number of demand loads

DEMAND <- setRefClass("DEMAND", 
                   fields = list(data = "matrix",
                                 n = "numeric"),
                   methods = list(
                       initialize = function(data, n){
                           data <<- matrix();
                           n <<- numeric()
                       }
                       
                   ))