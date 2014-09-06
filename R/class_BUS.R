#' The class BUS 
#'
#' @field data bus data
#' @field n total number of buses
#' @field index bus indexes
#' @field Pg active power injected in the network by generators
#' @field Qg reactive power injected in the network by generators
#' @field Pl active power absorbed from the network by loads
#' @field Ql reactive power absorbed from the network by loads
#' @field names bus names

BUS <- setRefClass("BUS", 
                   fields = list(data = "matrix",
                                 n = "numeric",
                                 index = "numeric",
                                 Pg = "numeric",
                                 Qg = "numeric",
                                 Pl = "numeric",
                                 Ql = "numeric",
                                 names = "character"),
                    methods = list(
                        initialize = function(data, n, index, Pg, Qg, Pl, Ql, names){
                            data <<- matrix();
                            n <<- numeric();
                            index <<- numeric();
                            Pg <<- numeric();
                            Qg <<- numeric();
                            Pl <<- numeric();
                            Ql <<- numeric();
                            names <<- character();
                        }
                        
                        ))