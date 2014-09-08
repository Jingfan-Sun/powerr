#' The class LINE 
#'
#' @field n total number of lines
#' @field Y admittance matrix of the network
#' @field from indexes of buses at which lines begin
#' @field to indexes of buses at which lines end

LINE <- setRefClass("LINE", contains = "powerr", 
                   fields = list(n = "numeric",
                                 Y = "matrix",
                                 fr = "numeric",
                                 to = "numeric",
                                 vfr = "numeric",
                                 vto = "numeric",
                                 p = "numeric",
                                 q = "numeric",
                                 nu = "numeric"),
                   methods = list(
                       initialize = function(data, n, Y, fr, to, vfr, vto, nu){
                           n <<- numeric();
                           Y <<- matrix();
                           fr <<- numeric();
                           to <<- numeric();
                           vfr <<- numeric();
                           vto <<- numeric();
                           p <<- numeric();
                           q <<- numeric();
                           ncol <<- 16;
                           nu <<- 16;
                       },
                       setup = function(Bus){
                           n <<- nrow(data);
                           ncolTemp <- ncol(data);
                           fr <<- Bus$getbus(data[, 1])[[1]];
                           vfr <<- Bus$getbus(data[, 1])[[2]];
                           to <<- Bus$getbus(data[, 2])[[1]];
                           vto <<- Bus$getbus(data[, 2])[[2]];
                           
                           p <<- Bus$getzeros();
                           q <<- Bus$getzeros();
                           
                           # size control
                           if (ncolTemp < ncol){
                               cBind(data, matrix(0, nrow = n, ncol = (ncol - ncolTemp)));
                           }
                           if (ncolTemp > ncol){
                               data <<- data[, 1:ncol];
                           }
                           
                           store <<- data;
                       },
                       gcall = function(){
                           DAE$x
                       }
                   ))