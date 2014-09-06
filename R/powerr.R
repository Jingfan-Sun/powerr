#' Initialize varables and classes for later analysis
#' 
#' @examples
#' powerInit()

powerInit <- function(){
    
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


