#' The class PQ 
#'
#' @field n total number of PQ generators
#' @field bus numbers of buses to which PQ generators are connected
#' @field gen 1 if it is a PQ generator, 0 otherwise.
#' @field P0 initial active power (used with non-conventional loads)
#' @field Q0 initial reactive power (used with non-conventional loads)

PQ <- setRefClass("PQ", contains = "powerr", 
                  fields = list(n = "numeric",
                                bus = "numeric",
                                vbus = "numeric",
                                P0 = "numeric",
                                Q0 = "numeric",
                                vmax = "numeric",
                                vmin = "numeric",
                                shunt = "numeric",
                                gen = "numeric"),
                  methods = list(
                      initialize = function(data, n, bus, vbus, P0, Q0, vmax, vmin, shunt, gen) {
                          n <<- numeric();
                          bus <<- numeric();
                          vbus <<- numeric();
                          P0 <<- numeric();
                          Q0 <<- numeric();
                          vmax <<- numeric();
                          vmin <<- numeric();
                          shunt <<- numeric();
                          gen <<- numeric();
                          ncol <<- 9;
                      },
                      setup = function(Bus){
                          if (length(data) == 0) {
                              # do nothing ...
                              temp <- 1;
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
                              gen <<- rep(0, n);
                              shunt <<- rep(0, n);
                              
                              if (length(data[1, ]) == 7){
                                  data <<- cBind(data, matrix(0, nrow = n, ncol = 1), matrix(1, nrow = n, ncol = 1));
                              } else if (length(data[1, ]) == 8){
                                  data <<- cBind(data, matrix(1, nrow = n, ncol = 1));
                              } else {
                                  stop('PQ data format is not consistent');
                              }
                              
                              if (length(data[1, ]) < ncol){
                                  u <<- rep(1, n);
                              }else {
                                  u <<- data[, ncol];
                              }
                              
                              idx <- powerFind(data[, 6] <= 0, 0);
                              if (length(idx) != 0) data[ide, 6] <<- 1.2;
                              idx <- powerFind(data[, 7] <= 0, 0);
                              if (length(idx) != 0) data[ide, 7] <<- 0.8;
                              
                              P0 <<- u * data[, 4];
                              Q0 <<- u * data[, 5];
                              vmax <<- rep(1, n);
                              vmin <<- rep(1, n);
                              store <<- data;
                          }
                      }, 
                      addgen = function(a, b, Bus) {
                          # defalut PQgen call this method, so parameters can be changed use '<<-'
                          # Thus, the return variable is PQload which should be returned to change
                          if (length(b$n) == 0) {return(b);}
                          else {
                              # set generated power as negtive loads
                              
                              return(b);
                          }
                      },
                      gcall = function(DAE) {
                          if (length(n) == 0){
                              # do nothing
                              return(DAE);
                          } else {
                              DAE$g[bus] <- u * data[, 4] + DAE$g[bus];
                              DAE$g[vbus] <- u * data[, 5] + DAE$g[vbus];
                              
                              a <- which((DAE$y[vbus] < data[, 7] & data[, 8] & u) | shunt);
                              b <- which(DAE$y[vbus] > data[, 6] & data[, 8] & u);
                              
                              if (length(a) != 0) {
                                  k <- bus[a];
                                  h <- vbus[a];
                                  DAE$g[k] <- data[a, 4] * DAE$y[h] * DAE$y[h] / data[a, 7] / data[a, 7] +
                                      DAE$g[k] - data[a, 4];
                                  DAE$g[h] <- data[a, 5] * DAE$y[h] * DAE$y[h] / data[a, 7] / data[a, 7] +
                                      DAE$g[h] - data[a, 5];
                              }
                              
                              if (length(b) != 0) {
                                  k <- bus[b];
                                  h <- vbus[b];
                                  DAE$g[k] <- data[b, 4] * DAE$y[h] * DAE$y[h] / data[b, 7] / data[b, 7] +
                                      DAE$g[k] - data[b, 4];
                                  DAE$g[h] <- data[b, 5] * DAE$y[h] * DAE$y[h] / data[b, 7] / data[b, 7] +
                                      DAE$g[h] - data[b, 5];
                              }
                              
                              return(DAE);
                          }
                      },
                      Gycall = function(DAE) {
                          if (length(n) == 0){
                              # do nothing
                              return(DAE);
                          } else {
                              a <- which((DAE$y[vbus] < data[, 7] & data[, 8] & u) | shunt);
                              b <- which(DAE$y[vbus] > data[, 6] & data[, 8] & u);
                              
                              if (length(a) != 0) {
                                  k <- bus[a];
                                  h <- vbus[a];
                                  DAE$Gy <- DAE$Gy + 
                                      powerDenseMatrix(h, k, 2 * data[, 4] * DAE$y[k] / data[, 7] / 
                                                           data[, 7], c(DAE$m, DAE$m));
                                  DAE$Gy <- DAE$Gy + 
                                      powerDenseMatrix(k, k, 2 * data[, 5] * DAE$y[k] / data[, 7] / 
                                                           data[, 7], c(DAE$m, DAE$m));
                              }
                              
                              if (length(b) != 0) {
                                  k <- bus[b];
                                  h <- vbus[b];
                                  DAE$Gy <- DAE$Gy + 
                                      powerDenseMatrix(h, k, 2 * data[, 4] * DAE$y[k] / data[, 6] / 
                                                           data[, 6], c(DAE$m, DAE$m));
                                  DAE$Gy <- DAE$Gy + 
                                      powerDenseMatrix(k, k, 2 * data[, 5] * DAE$y[k] / data[, 6] / 
                                                           data[, 6], c(DAE$m, DAE$m));
                              }
                              
                              return(DAE);
                          }
                      }
                      
                  ))