#' solve standard power flow with Newton–Raphson method
#' 
#' @param method 'newton' for standard Newton–Raphson method
#' @param tolerance set minimum accuracy for the power flow run
#' @param iterLimit set maximum iteration for each run 

powerPF <- function(method = 'newton', tolerance = 1e-5, iterLimit = 20){
        
    # Setup components
    powerComponentsSetup();
    
    # Build Admittance Matrix in Line
    Line$buildAdmittance(Bus);
    
    # memory allocation for equations and Jacobians
    DAE$f <- 0;
    DAE$x <- 0;
    DAE$Fx <- 1;
#     DAE$Fy <- Matrix(0, nrow = 1, ncol = DAE$m, sparse = TRUE);
#     DAE$Gx <- Matrix(0, nrow = DAE$m, ncol = 1, sparse = TRUE);
    DAE$Fy <- matrix(0, nrow = 1, ncol = DAE$m)
    DAE$Gx <- matrix(0, nrow = DAE$m, ncol = 1);
    
    convergence <- 1;
    iteration <- 0;
    err_max <- tolerance + 1;
    err_max_old <- 1e6;
    alfa <- 1;
    
    # start timer
    timeStart <- proc.time();
    
    if (method == 'newton'){
        print('Power Flow Solver: Newton–Raphson method');
        
        while ((err_max > tolerance) & (iteration <= iterLimit) & (alfa > 1e-5)){
            
        }
    }
    
    proc.time() - timeStart;
}

#' calculate the increase of each iteration in each run
#' 

calcInc <- function(){
    
    DAE.g = rep(0, DAE.m);
    
    # single slack bus
    
    
}