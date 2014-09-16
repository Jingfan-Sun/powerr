#' SUPPLY
#' 
#' The class SUPPLY 
#'
#' @field n total number of supply

SUPPLY <- setRefClass("SUPPLY", contains = "powerr", 
                      fields = list(n = "numeric", 
                                    bus = "numeric"),
                      methods = list(
                          initialize = function(data, n){
                              n <<- numeric();
                              bus <<- numeric();
                              ncol <<- 20;
                          },
                          setup = function(){
                              if (length(data) == 0) {
                                  # do nothing
                                  temp <- 1;
                              } else {
                                  n <<- length(data[, 1]);
                                  bus <<- Bus$getint(data[, 1]);
                                  
                                  nsup <- length(data[1, ]);
                                  if (nsup < 14) {
                                      data <<- cBind(data, matrix(0, nrow = n, ncol = (14 - nsup)));
                                  }
                                  
                                  if (length(data[1, ]) == 19){
                                      data <<- cBind(data, matrix(1, nrow = n, ncol = 1));
                                  } else if (length(data[1, ]) == 18){
                                      data <<- cBind(data, data[, 8], matrix(1, nrow = n, ncol = 1));
                                  } else if (length(data[1, ]) == 17){
                                      data <<- cBind(data, data[, 8], data[, 8], matrix(1, nrow = n, ncol = 1));
                                  } else if (length(data[1, ]) == 16){
                                      data <<- cBind(data, matrix(1, n, 1), data[, 8], data[, 8], matrix(1, n, 1));
                                  }
                                  
                                  u <<- data[, ncol];
                                  store <<- data;
                              }
                          }
                      ))