#' The class LINE 
#'
#' @field n total number of lines
#' @field Y admittance matrix of the network
#' @field from indexes of buses at which lines begin
#' @field to indexes of buses at which lines end

LINE <- setRefClass("LINE", contains = "powerr", 
                   fields = list(n = "numeric",
                                 Y = "matrix",
                                 from = "numeric",
                                 to = "numeric"),
                   methods = list(
                       initialize = function(data, n, Y, from, to){
                           n <<- numeric();
                           Y <<- matrix();
                           from <<- numeric();
                           to <<- numeric();
                           ncol <<- 16;
                       },
                       gcall = function(){
                           DAE$x
                       }
                   ))