#' The class PQ 
#'
#' @field data PQ generator data
#' @field n total number of PQ generators
#' @field bus numbers of buses to which PQ generators are connected
#' @field gen 1 if it is a PQ generator, 0 otherwise.

PQ <- setRefClass("PQ", 
                  fields = list(data = "matrix",
                                n = "numeric",
                                bus = "numeric",
                                gen = "logical"),
                  methods = list(
                      initialize = function(data, n, bus, gen){
                          data <<- matrix();
                          n <<- numeric();
                          bus <<- numeric();
                          gen <<- logical()
                      }
                      
                  ))