#' top class of package powerr
#' 
#' The top class powerr 
#'
#' @field data powerr data
#' @field store store temp for data
#' @field ncol number of the column of the data input
#' @field u connection status

powerr <- setRefClass("powerr", 
                   fields = list(data = "matrix",
                                 store = "matrix",
                                 ncol = "numeric",
                                 u = "numeric"),
                   methods = list(
                       initialize = function(data, store, ncol){
                           data <<- matrix();
                           store <<- matrix();
                           ncol <<- numeric();
                           u <<- numeric();
                       }
                       
                   ))