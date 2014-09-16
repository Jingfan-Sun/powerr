#' SG
#' 
#' The class SG (Slack Generator)
#'
#' @field n total number of slack generators
#' @field bus indexes of buses to which slack generators are connected
#' @field vbus indexes of voltage buses of slack generators
#' @field refbus indexes of buses used as phase reference

SG <- setRefClass("SG", contains = "powerr", 
                   fields = list(n = "numeric",
                                 bus = "numeric",
                                 vbus = "numeric",
                                 refbus = "numeric",
                                 qmax = "numeric",
                                 qmin = "numeric",
                                 pg = "numeric",
                                 qg = "numeric",
                                 dq = "numeric"),
                   methods = list(
                       initialize = function(data, n, bus, vbus, refbus, qmax, qmin, pg, qg, dq){
                           n <<- numeric();
                           bus <<- numeric();
                           vbus <<- numeric();
                           refbus <<- numeric();
                           qmax <<- numeric();
                           qmin <<- numeric();
                           pg <<- numeric();
                           qg <<- numeric();
                           dq <<- numeric();
                           ncol <<- 13;
                       },
                       setup = function(){
                           if (length(data) == 0) {
                               # do nothing
                           } else {
                               n <<- length(data[, 1]);
                               bus <<- Bus$getbus(data[, 1])[[1]];
                               vbus <<- Bus$getbus(data[, 1])[[2]];
                               
                               b <- unique(bus);
                               if (n > length(b)) {
                                   stop('More than one slack generator connected to the same bus.');
                               }
                               
                               if (length(data[1, ]) == 9){
                                   data <<- cBind(data, matrix(0, nrow = n, ncol = 1), matrix(1, nrow = n, ncol = 3));
                               } else if (length(data[1, ]) == 10){
                                   data <<- cBind(data, matrix(1, nrow = n, ncol = 3));
                               } else if (length(data[1, ]) == 11){
                                   data <<- cBind(data, matrix(1, nrow = n, ncol = 2));
                               } else if (length(data[1, ]) == 12){
                                   data <<- cBind(data, matrix(1, nrow = n, ncol = 1));
                               }
                               
                               z <- data[, 12];
                               u <<- data[, ncol];
                               
                               # at least one angle must be reference
                               # at least one bus must be the slack
                               
                               .GlobalEnv$DAE$y[vbus] <- data[, 4];
                               if (!sum(.GlobalEnv$DAE$y[Bus$a]) & n == 1) {
                                   .GlobalEnv$DAE$y[Bus$a] <- data[1, 5];
                               } else {
                                   .GlobalEnv$DAE$y[bus] <- data[, 5];
                               }
                               
                               # checking the consistency of distributed slack bus
                               
                               refbus <<- bus[powerFind((z & u), 0)];
                               pg <<- data[, 10];
                               dq <<- rep(0, n);
                               qg <<- rep(0, n);
                               qmax <<- rep(1, n);
                               qmin <<- rep(1, n);
                               store <<- data;
                           }
                           
                       },
                       gcall = function() {
                           if (length(n) == 0){
                               # do nothing
                           } else {
                               idx <- which(u != 0);
                               .GlobalEnv$DAE$g[bus[idx]] <- 0;
                               if (.GlobalEnv$Settings$pv2pq == FALSE) {
                                   .GlobalEnv$DAE$g[vbus[idx]] <- 0;
                               }
                           }
                           
                       },
                       Gycall = function() {
                           if (length(n) == 0){
                               # do nothing
                           } else {
                               
                               setgy(idx = bus[which(u != 0)]);
                               
                               if (.GlobalEnv$Settings$pv2pq == TRUE) {
                                   setgy(idx = vbus[which((!pq & u) != 0)]);
                               } else {
                                   setgy(idx = vbus[which(u != 0)]);
                               }
                           }
                           
                       },
                       Fxcall = function(type = 'all') {
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
                               
                               if (type == 'onlyq') {
                                   # do nothing
                               } else {
                                   idx <- bus[which(u != 0)];
                                   .GlobalEnv$DAE$Fy[, idx] <- 0;
                                   .GlobalEnv$DAE$Gx[idx, ] <- 0;
                               }
                           }
                           
                       }
                       
                   ))