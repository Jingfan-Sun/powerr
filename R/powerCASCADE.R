#' powerCASCADE
#' 
#' analyse and plot PDF for CASCADE failing model
#' 
#' @param L load stress
#' @param n total number of motors connected to load bus
#' @param D size of initial disturbance of CASCADE model
#' @param Q the amount of propagation in CASCADE model
#' @param N number of iteration
#' 
#' @examples
#' powerCASCADE()

powerCASCADE <- function(L = 0.5, n = 1000, D = 0.0004, Q = 0.0004, N = 10000){
    
    # start timer
    timeStart <- proc.time();
    
    d <- D / (2 - 2 * L)
    q <- Q / (2 - 2 * L)
    PDF <- numeric(n)
    #S <- 0
    
    apply(as.matrix(1:N), 1, function(x) {
        x <- runif(n, 0, 1)
        
        m <- sum(x > (1 - d))
        up <- 1 - d
        M <- c(0, m)
        
        while(m > 0) {          
            down <- up - m * q
            m <- sum(x > down & x <= up)
            up <- down
            M <- c(M, m)
        }
        PDF[sum(M)] <<- PDF[sum(M)] + 1
    })
    
    PDF <- PDF / N
    PDF[which(PDF == 0)] <- NA;
    plot(PDF, pch=20, log="xy")
    
    # print elapsed time in unit second
    cat('Time elapsed:', as.list(proc.time() - timeStart)$elapsed, 's');
}