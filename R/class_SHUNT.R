#' The class SHUNT 
#'
#' @field data shunt impedance data.
#' @field bus numbers of buses to which shunt are connected
#' @field g column vector of the conductances at each bus of the network
#' @field b column vector of the susceptances at each bus of the network

SHUNT <- setRefClass("SHUNT", contains = "powerr", 
                     fields = list(bus = "numeric",
                                   vbus = "numeric",
                                   n = "numeric",
                                   g = "numeric",
                                   b = "numeric"),
                     methods = list(
                         initialize = function(data, bus, vbus, n, g, b){
                             bus <<- numeric();
                             vbus <<- numeric();
                             n <<- 0;
                             g <<- numeric();
                             b <<- numeric();
                             ncol <<- 7;
                         },
                         setup = function(){
                             
                             if (length(data) == 0) {
                                 # do nothing
                             } else {
                                 n <<- nrow(data);
                                 bus <<- Bus$getbus(data[, 1])[[1]];
                                 vbus <<- Bus$getbus(data[, 1])[[2]];
                                 
                                 if (length(data[1, ]) < ncol){
                                     u <<- rep(1, n);
                                 }else {
                                     u <<- data(, ncol);
                                 }
                                 
                                 store <<- data;
                             }
                         },
                         gcall = function() {
                             if (length(n) == 0){
                                 # do nothing
                             } else {
                                 V <- DAE$y[vbus];
                                 V2 <- u * V * V;
                                 
                                 DAE$g <- DAE$g +
                                     powerDenseMatrix(bus, 1, data[, 5] * V2, c(DAE$m, 1)) -
                                     powerDenseMatrix(vbus, 1, data[, 6] * V2, c(DAE$m, 1));
                             }
                             assign("DAE", DAE, envir = .GlobalEnv);
                         },
                         Gycall = function() {
                             if (length(n) == 0){
                                 # do nothing
                             } else {
                                 V <- 2 * u * DAE$y[vbus];
                                 
                                 DAE$Gy <- DAE$Gy + 
                                     powerDenseMatrix(bus, vbus, data[, 5] * V, c(DAE$m, DAE$m)) - 
                                     powerDenseMatrix(vbus, vbus, data[, 6] * V, c(DAE$m, DAE$m));
                             }
                             assign("DAE", DAE, envir = .GlobalEnv);
                         }
                         
                     ))