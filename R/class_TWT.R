#' TWT
#' 
#' The class TWT (Two Winds Transformer)
#'

TWT <- setRefClass("TWT", contains = "powerr", 
                    methods = list(
                        initialize = function(data){
                            ncol <<- 25;
                        }
                        
                    ))