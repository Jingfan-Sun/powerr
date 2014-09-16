#' DEMAND
#' 
#' The class DEMAND 
#'
#' @field n total number of demand loads

DEMAND <- setRefClass("DEMAND", contains = "powerr", 
                   fields = list(n = "numeric",
                                 bus = "numeric",
                                 vbus = "numeric"),
                   methods = list(
                       initialize = function(data, n){
                           n <<- numeric();
                           bus <<- numeric();
                           vbus <<- numeric();
                           ncol <<- 18;
                       },
                       setup = function() {
                           if (length(data) == 0) {
                               # do nothing
                               temp <- 1;
                           } else {
                               n <<- length(data[, 1]);
                               bus <<- Bus$getbus(data[, 1])[[1]];
                               vbus <<- Bus$getbus(data[, 1])[[2]];
                               
                               if (length(data[1, ]) < ncol) {
                                   data[, ncol] <<- rep(1, n);
                               }
                               
                               u <<- data[, ncol];
                               store <<- data;
                           }
                       }
                   ))