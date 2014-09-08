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
                          if (length(data) == 0) stop();
                          
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
                      }, 
                      addgen = function(a, b, Bus) {
                          if (length(b$n) == 0) return(list(a, b));
                          
                          # set generated power as negtive loads
                          
                          
                          return(list(a, b));
                          
                      }
                      
                  ))