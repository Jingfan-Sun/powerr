#' The class DEMAND 
#'
#' @field n total number of demand loads

DEMAND <- setRefClass("DEMAND", contains = "powerr", 
                   fields = list(n = "numeric"),
                   methods = list(
                       initialize = function(data, n){
                           n <<- numeric()
                           ncol <<- 18;
                       }
                       
                   ))