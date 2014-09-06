#' The class LINE 
#'
#' @field data transmission line data
#' @field n total number of lines
#' @field Y admittance matrix of the network
#' @field from indexes of buses at which lines begin
#' @field to indexes of buses at which lines end

LINE <- setRefClass("LINE", 
                   fields = list(data = "matrix",
                                 n = "numeric",
                                 Y = "matrix",
                                 from = "numeric",
                                 to = "numeric"),
                   methods = list(
                       initialize = function(data, n, Y, from, to){
                           data <<- matrix();
                           n <<- numeric();
                           Y <<- matrix();
                           from <<- numeric();
                           to <<- numeric();
                       }
                       
                   ))