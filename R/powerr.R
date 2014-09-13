#' Initialize varables and classes for later analysis
#' 
#' @examples
#' powerInit()

powerInit <- function(){
    rm(list = ls(.GlobalEnv), envir = .GlobalEnv);
    
    # globle settings
    Settings <<- list(pv2pq = FALSE,
                      show = TRUE,
                      sparse = FALSE);
    
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
    
    powerVarInit();
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
            .GlobalEnv$DAE$Gy[idx, ] <- 0;
            .GlobalEnv$DAE$Gy[, idx] <- 0;
            .GlobalEnv$DAE$Gy <- .GlobalEnv$DAE$Gy + powerMatrix(idx, idx, 1, c(.GlobalEnv$DAE$m, .GlobalEnv$DAE$m));
        } else {
            .GlobalEnv$DAE$Gy[idx, ] <- 0;
            .GlobalEnv$DAE$Gy[, idx] <- 0;
            .GlobalEnv$DAE$Gy <- .GlobalEnv$DAE$Gy + powerMatrix(idx, idx, 1, c(.GlobalEnv$DAE$m, .GlobalEnv$DAE$m));
            .GlobalEnv$DAE$Fy[, idx] <- 0;
            .GlobalEnv$DAE$Gx[idx, ] <- 0;
        }
    }
}

#' initialize ref class variables
#' used seperately to implement new Settings
#' 

powerVarInit <- function() {
    .GlobalEnv$Bus <- BUS$new();
    .GlobalEnv$Line <- LINE$new();
    .GlobalEnv$PQload <- PQ$new();
    .GlobalEnv$PVgen <- PV$new();
    .GlobalEnv$Twt <- TWT$new();
    .GlobalEnv$Shunt <- SHUNT$new();
    .GlobalEnv$Slack <- SG$new();
    .GlobalEnv$Demand <- DEMAND$new();
    .GlobalEnv$Supply <- SUPPLY$new();
    .GlobalEnv$PQgen <- PQ$new();
}

