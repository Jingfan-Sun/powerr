#' The class TWT 
#'

TWT <- setRefClass("TWT", contains = "powerr", 
                    methods = list(
                        initialize = function(data){
                            ncol <<- 25;
                        }
                        
                    ))