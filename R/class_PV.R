#' The class PV 
#'
#' @field data PV generator data
#' @field n total number of PV generators
#' @field bus numbers of buses to which PV generators are connected
#' @field pq internal PQ bus data (used when generator reactive power limits are
#' encountered)

PV <- setRefClass("PV", 
                   fields = list(data = "matrix",
                                 n = "numeric",
                                 bus = "numeric",
                                 pq = "numeric"),
                   methods = list(
                       initialize = function(data, n, bus, pq){
                           data <<- matrix();
                           n <<- numeric();
                           bus <<- numeric();
                           pq <<- numeric()
                       }
                       
                   ))