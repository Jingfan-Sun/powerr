#' powerInit
#' 
#' Initialize varables and classes for later analysis
#' 
#' @examples
#' powerInit()

powerInit <- function(){
    rm(list = ls(.GlobalEnv), envir = .GlobalEnv);
    
    Settings = DAE = NULL;
    
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

#' powerData
#'
#' Read formated data into powerr
#' 
#' @param data name of data file with '.R' extension
#' @param path the path of the datafile with no '/' in the end
#' 
#' @examples
#' \donttest{
#' powerData('d_003.R')
#' }

powerData <- function(data = 'd_003.R', 
                      path = paste(path.package('powerr'), '/extdata', sep = '')){
    # check file exist
    if (!(file.exists(paste(path, '/', data, sep = '')))) {
        stop('No file found in path provided');
    }
    # read data
    source(paste(path, '/', data, sep = ''))
}

#' setgy
#' 
#' set gy field in powerr classes
#' 
#' @param idx index of gy components
#' @param type distinguish different type of gy setup

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

#' powerVarInit
#' 
#' initialize ref class variables
#' used seperately to implement new Settings
#' 
#' @examples
#' powerVarInit()

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

