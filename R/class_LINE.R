#' LINE
#' 
#' The class LINE 
#'
#' @field n total number of lines
#' @field from indexes of buses at which lines begin
#' @field to indexes of buses at which lines end

LINE <- setRefClass("LINE", contains = "powerr", 
                    fields = list(n = "numeric",
                                  fr = "numeric",
                                  to = "numeric",
                                  vfr = "numeric",
                                  vto = "numeric",
                                  p = "numeric",
                                  q = "numeric",
                                  nu = "numeric"),
                    methods = list(
                        initialize = function(data, n, fr, to, vfr, vto, nu){
                            n <<- numeric();
                            fr <<- numeric();
                            to <<- numeric();
                            vfr <<- numeric();
                            vto <<- numeric();
                            p <<- numeric();
                            q <<- numeric();
                            ncol <<- 16;
                            nu <<- 16;
                        },
                        setup = function(){
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
                            if (length(n) == 0){
                                # do nothing
                            } else {
                                .GlobalEnv$DAE$g <- rep(0, .GlobalEnv$DAE$m);
                                
                                na <- Bus$a;
                                nv <- Bus$v;
                                
                                .GlobalEnv$DAE$y[nv] <- apply(cBind(.GlobalEnv$DAE$y[nv], rep(1e-6, length(nv))), 1, max);
                                Vc <- .GlobalEnv$DAE$y[nv] * exp(1i * .GlobalEnv$DAE$y[na]);
                                S <- Vc %.*% (powerConj(.GlobalEnv$Y %**% Vc));
                                p <<- as.numeric(powerRe(S));
                                q <<- as.numeric(powerIm(S));
                                
                                .GlobalEnv$DAE$g[na] <- p;
                                .GlobalEnv$DAE$g[nv] <- q;
                            }
                            
                        },
                        # build admittance matrix
                        buildAdmittance = function(){
                            if (length(n) == 0){
                                # do nothing
                                temp <- 0;
                            } else {
                                nb <- Bus$n;
                                # process line data and build admittance matrix [.GlobalEnv$Y]
                                chrg <- 0.5 * u * data[, 10];
                                y <- u / (data[, 8] + 1i * data[, 9]);
                                ts <- data[, 11] * exp(1i * data[, 12] * pi / 180);
                                ts2 <- ts * Conj(ts);
                                
                                #                                 .GlobalEnv$Y <- sparseMatrix(fr, to, x = (-y * ts), dims = c(nb, nb)) + 
                                #                                     sparseMatrix(to, fr, x = (-y * Conj(ts)), dims = c(nb, nb)) + 
                                #                                     sparseMatrix(fr, fr, x = (y + 1i * chrg), dims = c(nb, nb)) + 
                                #                                     sparseMatrix(to, to, x = (y * ts2 + 1i * chrg), dims = c(nb, nb));
                                
                                .GlobalEnv$Y <- powerMatrix(fr, to, x = (-y * ts), dims = c(nb, nb)) %++% 
                                    powerMatrix(to, fr, x = (-y * Conj(ts)), dims = c(nb, nb)) %++% 
                                    powerMatrix(fr, fr, x = (y + 1i * chrg), dims = c(nb, nb)) %++% 
                                    powerMatrix(to, to, x = (y * ts2 + 1i * chrg), dims = c(nb, nb));
                                
                                # check for missing connections (0 diagonal elements)
                                b <- which(powerDiag(.GlobalEnv$Y) == 0);
                                if (length(b) != 0) {
                                    #                                     .GlobalEnv$Y <- .GlobalEnv$Y - sparseMatrix(b, b, x = (1i * 1e-6), dims = c(nb, nb));
                                    .GlobalEnv$Y <- .GlobalEnv$Y %--% powerMatrix(b, b, x = (1i * 1e-6), dims = c(nb, nb));
                                }
                            }
                        },
                        Gycall = function() {
                            
                            build_gy();
                            
                        },
                        build_gy = function() {
                            .GlobalEnv$DAE$Gy <- 1e-6 * diag(1,.GlobalEnv$DAE$m, .GlobalEnv$DAE$m);
                            nb <- Bus$n;
                            
                            if (length(n) == 0){
                                # do nothing
                            } else {
                                n1 <- Bus$a;
                                U <- exp(1i * .GlobalEnv$DAE$y[n1]);
                                V <- .GlobalEnv$DAE$y[Bus$v] %.*% U;
                                I <- .GlobalEnv$Y %**% V;
                                
                                diagVc <- powerMatrix(n1, n1, V, c(nb, nb));
                                diagVn <- powerMatrix(n1, n1, U, c(nb, nb));
                                diagIc <- powerMatrix(n1, n1, I, c(nb, nb));
                                dS <- (diagVc %**% powerConj(.GlobalEnv$Y %**% diagVn)) %++% (powerConj(diagIc) %**% diagVn);
                                dR <- powerConj(diagVc) %**% (diagIc %--% (.GlobalEnv$Y %**% diagVc));
                                
                                a <- rBind(cBind(powerIm(dR), powerRe(dS)), cBind(powerRe(dR), powerIm(dS)));
                                if (.GlobalEnv$Settings$sparse == TRUE) {
                                    TmpX <- as(a, "dgTMatrix");
                                    h <- TmpX@i + 1;
                                    k <- TmpX@j + 1;
                                } else {
                                    h <- which(a != 0, arr.ind=T)[, 1];
                                    k <- which(a != 0, arr.ind=T)[, 2];
                                }
                                s <- as.vector(a[cBind(h, k)]);
                                
                                .GlobalEnv$DAE$Gy <- powerMatrix(h, k, s, c(.GlobalEnv$DAE$m, .GlobalEnv$DAE$m))
                            }
                            
                        }
                    ))