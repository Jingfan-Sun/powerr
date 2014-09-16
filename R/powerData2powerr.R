#' powerData2powerr
#' 
#' change data file to powerr .R file format
#' 
#' @param dataName name of data file to be changed format
#' @param dataPath path of the file with no '/' in the end
#' @param fromMode the former data format
#' 
#' @examples
#' powerData2powerr()

powerData2powerr <- function(dataName = 'd_006_mdl.m', 
                             dataPath = paste(path.package('powerr'), '/extdata', sep = ''),
                             fromMode = 'psat') {
    path <- paste(dataPath, '/', dataName, sep = '');
    if (fromMode == 'psat') {
        inputData <- scan(path, what = character(0), sep = "\n");
        #change data name
        inputData <- gsub('SW', 'Slack', inputData);
        inputData <- gsub('PV', 'PVgen', inputData);
        inputData <- gsub('PQ', 'PQload', inputData);
        # change format
        inputData <- gsub('  ', ',', inputData);
        inputData <- gsub(' = ', ' <- ', inputData);
        inputData <- gsub('.con', '$data', inputData);
        inputData <- gsub('.names', '$names', inputData);
        inputData <- sub(',', '', inputData);
        inputData <- gsub("\\{...", 'c(', inputData);
        inputData <- gsub(";", ',', inputData);
        inputData <- gsub("\\},", ')', inputData);
        inputData <- gsub("\\[ ... ", 'matrix(c(', inputData);
        endIndex <- grep('\\],', inputData);
        col <- apply(as.matrix(endIndex - 1), 1, function(x) {
            a <- strsplit(inputData[x], '')[[1]];
            a[length(a)] <- '),';
            inputData[x] <<- paste(a, collapse = '');
            col <- length(strsplit(inputData[x], ',')[[1]]);
            return(col)
        })
        
        row <- apply(cbind(endIndex - 1, c(0, endIndex)[1:length(endIndex)]), 1, 
                     function(x) {
                         row <- x[[1]] - x[[2]] - 1;
                     })
        
        noUse <- apply(cbind(endIndex, row, col), 1, function(x) {
            inputData[x[[1]]] <<- gsub("\\],",
                                       paste(x[[2]], ', ', x[[3]], ', byrow = TRUE)', sep = ''),
                                       inputData[x[[1]]]);
            return(NULL);
        })
        dataNameR <- strsplit(dataName, '')[[1]];
        dataNameR[length(dataNameR)] <- 'R';
        writeLines(inputData, con = paste(dataPath, '/', paste(dataNameR, collapse = ''), sep = ''), sep = '\n')
    } else {
        # do nothing
    }
    
}