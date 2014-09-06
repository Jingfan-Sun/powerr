#' The class TWT 
#'
#' @field data three winding transformer data

TWT <- setRefClass("TWT", 
                    fields = list(data = "matrix"),
                    methods = list(
                        initialize = function(data){
                            data <<- matrix();
                        }
                        
                    ))