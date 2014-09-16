#' BUS
#' 
#' The class BUS 
#'
#' @field n total number of buses
#' @field index bus indexes
#' @field int internal nus number
#' @field Pg active power injected in the network by generators
#' @field Qg reactive power injected in the network by generators
#' @field Pl active power absorbed from the network by loads
#' @field Ql reactive power absorbed from the network by loads
#' @field names bus names

BUS <- setRefClass("BUS", contains = "powerr", 
                   fields = list(n = "numeric",
                                 index = "numeric",
                                 int = "numeric",
                                 a = "numeric",
                                 v = "numeric",
                                 Pg = "numeric",
                                 Qg = "numeric",
                                 Pl = "numeric",
                                 Ql = "numeric",
                                 names = "character"),
                   methods = list(
                       initialize = function(data, n, index, int, a, v, Pg, Qg, Pl, Ql, names, ncol){
                           n <<- numeric();
                           a <<- numeric();
                           v <<- numeric();
                           index <<- numeric();
                           int <<- numeric();
                           Pg <<- numeric();
                           Qg <<- numeric();
                           Pl <<- numeric();
                           Ql <<- numeric();
                           names <<- character();
                           ncol <<- 6;
                       },
                       setup = function(){
                           if (nrow(data) == 0 && ncol(data) == 0){
                               print('The data file does not seem to be in a valid format: no bus found$')
                           }
                           n <<- nrow(data);
                           a <<- 1:n;
                           v <<- a + n;
                           
                           # setup internal bus number for second indexing of bus
                           int[data[, 1]] <<- a;
                           
                           .GlobalEnv$DAE$m <- 2 * n;
                           .GlobalEnv$DAE$y <- rep(0, .GlobalEnv$DAE$m);
                           .GlobalEnv$DAE$g <- rep(0, .GlobalEnv$DAE$m);
                           #                            .GlobalEnv$DAE$Gy <- Matrix(0, nrow = .GlobalEnv$DAE$m, ncol = .GlobalEnv$DAE$m, sparse = TRUE);
                           .GlobalEnv$DAE$Gy <- matrix(0, nrow = .GlobalEnv$DAE$m, ncol = .GlobalEnv$DAE$m);
                           
                           if (ncol(data) >= 4){
                               
                               # check voltage magnitudes
                               if (sum(data[, 3] < 0.5) > 0){
                                   warning('some initial guess voltage magnitudes are too low.');
                               }
                               if (sum(data[, 3] > 1.5) > 0){
                                   warning('some initial guess voltage magnitudes are too high.');
                               }
                               .GlobalEnv$DAE$y[v] <- data[, 3];
                               
                               # check voltage phases
                               if (sum(data[, 4] < -1.5708) > 0){
                                   warning('some initial guess voltage phases are too low.');
                               }
                               if (sum(data[, 4] > 1.5708) > 0){
                                   warning('some initial guess voltage phases are too high.');
                               }
                               .GlobalEnv$DAE$y[a] <- data[, 4];
                           }
                           else{
                               .GlobalEnv$DAE$y[a] <- rep(0, n);
                               .GlobalEnv$DAE$y[v] <- rep(1, n);
                           }
                           
                           Pl <<- rep(0, n);
                           Ql <<- rep(0, n);
                           Pg <<- rep(0, n);
                           Qg <<- rep(0, n);
                       },
                       getbus = function(idx){
                           uTemp <- int[round(idx)];
                           vTemp <- uTemp + n;
                           return(list(uTemp, vTemp));
                       },
                       getzeros = function(){
                           uTemp <- rep(0, n);
                           return(uTemp);
                       },
                       getint = function(idx){
                           uTemp <- int[round(idx)];
                           return(uTemp);
                       },
                       test = function(){
                           print(environment())
                           print(parent.env(environment()))
                           print(parent.env(parent.env(environment())))
                           print(parent.env(parent.env(parent.env(environment()))))
                           print(parent.env(parent.env(parent.env(parent.env(environment())))))
                           .GlobalEnv$DAE$m <- 1000;
                           .GlobalEnv$DAE$n <- .GlobalEnv$DAE$m;
                           print(.GlobalEnv$DAE$n)
                       }
                       
                   ))