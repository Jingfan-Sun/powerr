#' The class SG 
#'
#' @field n total number of slack generators
#' @field bus indexes of buses to which slack generators are connected
#' @field vbus indexes of voltage buses of slack generators
#' @field refbus indexes of buses used as phase reference

SG <- setRefClass("SG", contains = "powerr", 
                   fields = list(n = "numeric",
                                 bus = "numeric",
                                 vbus = "numeric",
                                 refbus = "numeric"),
                   methods = list(
                       initialize = function(data, n, bus, vbus, refbus){
                           n <<- numeric();
                           bus <<- numeric();
                           vbus <<- numeric();
                           refbus <<- numeric();
                           ncol <<- 13;
                       }
                       
                   ))