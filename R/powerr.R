#' Initialize varables and classes for later analysis
#' 
#' @examples
#' powerInit()

powerInit <- function(){
    rm(list = ls())
    
    # globle settings
    Settings <<- list(pv2pq = FALSE,
                      show = TRUE)
    
    # Differential Algebraic Equations
    DAE <<- list(x = numeric(), 
                 y = numeric(), 
                 kg = 0, 
                 m = 0, 
                 n = 0, 
                 npf = 0, 
                 g = numeric(), 
                 f = numeric(), 
                 Gy = matrix(), 
                 Fx = matrix(), 
                 Fy = matrix(), 
                 Fl = matrix(), 
                 Fk = matrix(), 
                 Gx = matrix(), 
                 Gl = matrix(), 
                 Gk = matrix(), 
                 Ac = matrix(), 
                 tn = numeric(), 
                 t = -1
                 )
    
    Bus <<- BUS$new();
    Line <<- LINE$new();
    PQload <<- PQ$new();
    PVgen <<- PV$new();
    Twt <<- TWT$new();
    Shunt <<- SHUNT$new();
    Slack <<- SG$new();
    Demand <<- DEMAND$new();
    Supply <<- SUPPLY$new();
    PQgen <<- PQ$new();
}

#' Read and change data format into powerr format if needed
#' 

powerData <- function(data=NULL){
    # check file exist
    
    # read data
    source(paste(path.package('powerr'), '/extdata/', data, sep = ''))
}

#' setgy
#' 

setgy <- function(idx, type = 1) {
    if (length(idx) == 0) {
        # do nothing
    } else {
        if (type == 1) {
            DAE$Gy[idx, ] <- 0;
            DAE$Gy[, idx] <- 0;
            DAE$Gy <- DAE$Gy + powerDenseMatrix(idx, idx, 1, c(DAE$m, DAE$m));
        } else {
            DAE$Gy[idx, ] <- 0;
            DAE$Gy[, idx] <- 0;
            DAE$Gy <- DAE$Gy + powerDenseMatrix(idx, idx, 1, c(DAE$m, DAE$m));
            DAE$Fy[, idx] <- 0;
            DAE$Gx[idx, ] <- 0;
        }
    }
    assign("DAE", DAE, envir = .GlobalEnv);
}


