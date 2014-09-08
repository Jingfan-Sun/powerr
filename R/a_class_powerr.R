#' The top class powerr 
#'
#' @field data powerr data
#' @field ncol number of the column of the data input

powerr <- setRefClass("powerr", 
                   fields = list(data = "matrix",
                                 ncol = "numeric"),
                   methods = list(
                       initialize = function(data, n, index, Pg, Qg, Pl, Ql, names){
                           data <<- matrix();
                           ncol <<- numeric();
                       }
                       
                   ))