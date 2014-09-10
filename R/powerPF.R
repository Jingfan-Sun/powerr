#' solve standard power flow with Newton–Raphson method
#' 
#' @param method 'newton' for standard Newton–Raphson method
#' @param tolerance set minimum accuracy for the power flow run
#' @param iterLimit set maximum iteration for each run 

powerPF <- function(method = 'newton', tolerance = 1e-5, iterLimit = 20, DAE = DAE){
    
    # Setup components
    DAE <- powerComponentsSetup(DAE);
    
    # Build Admittance Matrix in Line
    Line$buildAdmittance(Bus);
    
    # memory allocation for equations and Jacobians
    # no dynamic components
    nodyn = TRUE;
    DAE$n <- 1;
    DAE$f <- 0;
    DAE$x <- 0;
    DAE$Fx <- 1;
    #     DAE$Fy <- Matrix(0, nrow = 1, ncol = DAE$m, sparse = TRUE);
    #     DAE$Gx <- Matrix(0, nrow = DAE$m, ncol = 1, sparse = TRUE);
    DAE$Fy <- matrix(0, nrow = 1, ncol = DAE$m);
    DAE$Gx <- matrix(0, nrow = DAE$m, ncol = 1);
    
    convergence <- 1;
    iteration <- 0;
    err_max <- tolerance + 1;
    err_max_old <- 1e6;
    err_vector <- numeric();
    alfa <- 1;
    
    # start timer
    timeStart <- proc.time();
    
    while ((err_max > tolerance) & (iteration <= iterLimit) & (alfa > 1e-5)){
        if (method == 'newton'){
            cat('Power Flow Solver: Newton–Raphson method');
            
            inc <- calcInc(nodyn, DAE)[[1]];
            DAE <- calcInc(nodyn, DAE)[[2]];
            
            DAE$x <- DAE$x + inc[1: DAE$n];
            DAE$y <- DAE$t + inc[(1 + DAE$n): (DAE$m + DAE$n)];
        }
        
        iteration <- iteration + 1;
        err_max <- max(abs(inc));
        err_vector <- c(err_vector, err_max);
        
        if (Settings$show == TRUE) {
            cat('Iteration:', iteration, ', Maximun Convergency Error:', err_max, '\n');
        }
        
        if (iteration > 4) {
            if (err_max > 1000 * err_vector[1]) {
                cat('The error is increasing too much.', '\n');
                cat('Convergence is likely not reachable.', '\n');
                convergence <- 0;
                break;
            }
        }
    }
    
    if (iteration > iter_max) {
        cat('Reach maximum number of iteration without convergence.', '\n');
        convergence <- 0;
    }
    
    # print elapsed time in unit second
    cat(as.list(proc.time() - timeStart)$elapsed);
    
    return(DAE);
}

#' calculate the increase of each iteration in each run
#' 

calcInc <- function(nodyn, DAE){
    
    DAE$g = rep(0, DAE$m);
    
    # single slack bus
    DAE <- Line$gcall(Bus, DAE);
    DAE <- PQload$gcall(DAE);
    DAE <- Shunt$gcall(DAE);
    DAE <- PVgen$gcall(Bus, DAE);
    DAE <- Slack$gcall(Bus, PVgen, DAE);
    DAE <- Line$Gycall(Bus, DAE);
    DAE <- PQload$Gycall(DAE);
    DAE <- Shunt$Gycall(DAE);
    DAE <- PVgen$Gycall(DAE);
    DAE <- Slack$Gycall(DAE);
    
    DAE$Fx <- matrix(0, DAE$n, DAE$n);
    DAE$Fy <- matrix(0, DAE$n, DAE$m);
    DAE$Gx <- matrix(0, DAE$m, DAE$n);
    
    if (nodyn == TRUE) {
        DAE$Fx <- 1;
    }
    
    DAE <- Slack$Fxcall(DAE, type = 'full');
    DAE <- PVgen$Fxcall(DAE);
    
    inc <- solve(-rbind(cbind(DAE$Fx, DAE$Fy), cbind(DAE$Gx, DAE$Gy)),
                 rbind(DAE$f, DAE$g));
    
    return(list(inc, DAE));
}