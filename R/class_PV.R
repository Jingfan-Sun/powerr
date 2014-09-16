#' PV
#' 
#' The class PV 
#'
#' @field n total number of PV generators
#' @field bus numbers of buses to which PV generators are connected
#' @field pq internal PQ bus data (used when generator reactive power limits are
#' encountered)

PV <- setRefClass("PV", contains = "powerr", 
                   fields = list(n = "numeric",
                                 bus = "numeric",
                                 vbus = "numeric",
                                 qmax = "numeric",
                                 qmin = "numeric",
                                 pq = "numeric",
                                 qg = "numeric"),
                   methods = list(
                       initialize = function(data, n, bus, vbus, qmax, qmin, pq, qg){
                           n <<- numeric();
                           bus <<- numeric();
                           vbus <<- numeric();
                           qmax <<- numeric();
                           qmin <<- numeric();
                           pq <<- numeric();
                           qg <<- numeric();
                           ncol <<- 11;
                       },
                       setup = function(){
                           if (length(data) == 0) {
                               # do nothing
                           } else {
                               bus <<- Bus$getint(data[, 1]);
                               k <- unique(bus);
                               h <- powerUnique(bus);
                               
                               if (length(k) > length(h)){
                                   warning('More than one PV generator connected o the same bus.');
                                   # ...
                               }
                               vbus <<- bus + Bus$n;
                               n <<- length(data[, 1]);
                               .GlobalEnv$DAE$y[vbus] <- data[, 5];
                               
                               if (length(data[1, ]) == 9){
                                   data <<- cBind(data, matrix(1, nrow = n, ncol = 2));
                               } else if (length(data[1, ]) == 10){
                                   data <<- cBind(data, matrix(1, nrow = n, ncol = 1));
                               }
                               
                               if (length(data[1, ]) < ncol){
                                   u <<- rep(1, n);
                               }else {
                                   u <<- data[, ncol];
                               }
                               
                               qmax <<- rep(1, n);
                               qmin <<- rep(1, n);
                               pq <<- rep(0, n);
                               qg <<- rep(0, n);
                               store <<- data;
                           }
                           
                           
                       },
                       gcall = function() {
                           if (length(n) == 0){
                               # do nothing
                           } else {
                               K <- u * (1 + .GlobalEnv$DAE$kg * data[, 10]);
                               .GlobalEnv$DAE$g[bus] <- .GlobalEnv$DAE$g[bus] - K * data[, 4];
                               
                               if (.GlobalEnv$Settings$pv2pq == FALSE) {
                                   .GlobalEnv$DAE$g[vbus[which(u != 0)]] <- 0;
                               }
                           }
                           
                       },
                       Gycall = function() {
                           if (length(n) == 0){
                               # do nothing
                           } else {
                               
                               if (.GlobalEnv$Settings$pv2pq == TRUE) {
                                   setgy(idx = vbus[which((!pq & u) != 0)]);
                               } else {
                                   setgy(idx = vbus[which(u != 0)]);
                               }
                           }
                           
                       },
                       Fxcall = function() {
                           if (length(n) == 0){
                               # do nothing
                           } else {
                               idx <- vbus[which(u != 0)];
                               
                               if (length(idx) == 0) {
                                   # do nothing
                               } else {
                                   .GlobalEnv$DAE$Fy[, idx] <- 0;
                                   .GlobalEnv$DAE$Gx[idx, ] <- 0;
                               }
                           }
                           
                       }
                       
                   ))