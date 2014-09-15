#' solve standard power flow with Newton–Raphson method
#' 
#' @param method 'newton' for standard Newton–Raphson method
#' @param tolerance set minimum accuracy for the power flow run
#' @param iterLimit set maximum iteration for each run 

powerPF <- function(method = 'newton', tolerance = 1e-5, iterLimit = 20){
    
    # Setup components
    powerComponentsSetup();
    
    # Build Admittance Matrix in Line
    Line$buildAdmittance();
    
    # memory allocation for equations and Jacobians
    # no dynamic components
    nodyn <- TRUE;
    .GlobalEnv$DAE$n <- 1;
    .GlobalEnv$DAE$f <- 0;
    .GlobalEnv$DAE$x <- 0;
    .GlobalEnv$DAE$Fx <- 1;
    .GlobalEnv$DAE$Fy <- Matrix(0, nrow = 1, ncol = .GlobalEnv$DAE$m, sparse = TRUE);
    .GlobalEnv$DAE$Gx <- Matrix(0, nrow = .GlobalEnv$DAE$m, ncol = 1, sparse = TRUE);
    #     .GlobalEnv$DAE$Fy <- powerMatrix(0, nrow = 1, ncol = .GlobalEnv$DAE$m);
    #     .GlobalEnv$DAE$Gx <- powerMatrix(0, nrow = .GlobalEnv$DAE$m, ncol = 1);
    
    convergence <- 1;
    iteration <- 0;
    err_max <- tolerance + 1;
    err_max_old <- 1e6;
    err_vector <- numeric();
    alfa <- 1;
    
    # start timer
    timeStart <- proc.time();
    
    if (method == 'newton') {
        cat('Power Flow Solver: Newton–Raphson method \n');
    }
    
    while ((err_max > tolerance) & (iteration <= iterLimit) & (alfa > 1e-5)){
        if (method == 'newton'){
            
            
            inc <- calcInc(nodyn);
            
            .GlobalEnv$DAE$x <- .GlobalEnv$DAE$x + inc[1: .GlobalEnv$DAE$n];
            .GlobalEnv$DAE$y <- .GlobalEnv$DAE$y + inc[(1 + .GlobalEnv$DAE$n): (.GlobalEnv$DAE$m + .GlobalEnv$DAE$n)];
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
    
    if (iteration > iterLimit) {
        cat('Reach maximum number of iteration without convergence.', '\n');
        convergence <- 0;
    }
    
    # print elapsed time in unit second
    cat('Time elapsed:', as.list(proc.time() - timeStart)$elapsed, 's');
    
}

#' calculate the increase of each iteration in each run
#' 

calcInc <- function(nodyn){
    
    .GlobalEnv$DAE$g <- rep(0, .GlobalEnv$DAE$m);
    
    # single slack bus
    Line$gcall();
    PQload$gcall();
    Shunt$gcall();
    PVgen$gcall();
    Slack$gcall();
    Line$Gycall();
    PQload$Gycall();
    Shunt$Gycall();
    PVgen$Gycall();
    Slack$Gycall();
    
    .GlobalEnv$DAE$Fx <- matrix(0, .GlobalEnv$DAE$n, .GlobalEnv$DAE$n);
    .GlobalEnv$DAE$Fy <- matrix(0, .GlobalEnv$DAE$n, .GlobalEnv$DAE$m);
    .GlobalEnv$DAE$Gx <- matrix(0, .GlobalEnv$DAE$m, .GlobalEnv$DAE$n);
    
    if (nodyn == TRUE) {
        .GlobalEnv$DAE$Fx <- 1;
    }
    
    Slack$Fxcall(type = 'full');
    PVgen$Fxcall();
    
    inc <- Matrix::solve(-rBind(cBind(.GlobalEnv$DAE$Fx, .GlobalEnv$DAE$Fy), cBind(.GlobalEnv$DAE$Gx, .GlobalEnv$DAE$Gy)),
                 rBind(.GlobalEnv$DAE$f, as.matrix(.GlobalEnv$DAE$g)));
    
    return(inc);
}