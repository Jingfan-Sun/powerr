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
                            
                            # status control
                            if (ncolTemp < nu) {
                                data <<- cBind(data, matrix(1, n, nu - ncolTemp));
                            }
                            
                            # set line status
                            u <<- data[, nu];
                            
                            # adjust tap ratio
                            if (length(which(abs(data[, 11]) == 0)) != 0) {
                                data[which(abs(data[, 11]) == 0), 11] <<- 1;
                            }
                            
                            store <<- data;
                        },
                        gcall = function(){
                            DAE$x
                        },
                        # build admittance matrix
                        buildAdmittance = function(Bus){
                            if (length(n) == 0){
                                # do nothing
                                temp <- 0;
                            } else {
                                nb <- Bus$n;
                                # process line data and build admittance matrix [Y]
                                chrg <- 0.5 * u * data[, 10];
                                y <- u / (data[, 8] + 1i * data[, 9]);
                                ts <- data[, 11] * exp(1i * data[, 12] * pi / 180);
                                ts2 <- ts * Conj(ts);
                                
#                                 Y <<- sparseMatrix(fr, to, x = (-y * ts), dims = c(nb, nb)) + 
#                                     sparseMatrix(to, fr, x = (-y * Conj(ts)), dims = c(nb, nb)) + 
#                                     sparseMatrix(fr, fr, x = (y + 1i * chrg), dims = c(nb, nb)) + 
#                                     sparseMatrix(to, to, x = (y * ts2 + 1i * chrg), dims = c(nb, nb));
                                
                                Y <<- powerDenseMatrix(fr, to, x = (-y * ts), dims = c(nb, nb)) + 
                                    powerDenseMatrix(to, fr, x = (-y * Conj(ts)), dims = c(nb, nb)) + 
                                    powerDenseMatrix(fr, fr, x = (y + 1i * chrg), dims = c(nb, nb)) + 
                                    powerDenseMatrix(to, to, x = (y * ts2 + 1i * chrg), dims = c(nb, nb));
                                
                                # check for missing connections (0 diagonal elements)
                                b <- which(diag(Y) == 0);
                                if (length(b) != 0) {
#                                     Y <<- Y - sparseMatrix(b, b, x = (1i * 1e-6), dims = c(nb, nb));
                                    Y <<- Y - powerDenseMatrix(b, b, x = (1i * 1e-6), dims = c(nb, nb));
                                }
                            }
                        }
                    ))