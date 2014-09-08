#' The class PV 
#'
#' @field n total number of PV generators
#' @field bus numbers of buses to which PV generators are connected
#' @field pq internal PQ bus data (used when generator reactive power limits are
#' encountered)

PV <- setRefClass("PV", contains = "powerr", 
                   fields = list(n = "numeric",
                                 bus = "numeric",
                                 pq = "numeric"),
                   methods = list(
                       initialize = function(data, n, bus, pq){
                           n <<- numeric();
                           bus <<- numeric();
                           pq <<- numeric();
                           ncol <<- 11;
                       }
                       
                   ))