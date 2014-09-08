#' The class SHUNT 
#'
#' @field data shunt impedance data.
#' @field bus numbers of buses to which shunt are connected
#' @field g column vector of the conductances at each bus of the network
#' @field b column vector of the susceptances at each bus of the network

SHUNT <- setRefClass("SHUNT", contains = "powerr", 
                  fields = list(bus = "numeric",
                                g = "numeric",
                                b = "numeric"),
                  methods = list(
                      initialize = function(data, bus, g, b){
                          bus <<- numeric();
                          g <<- numeric();
                          b <<- numeric();
                          ncol <<- 7;
                      }
                      
                  ))