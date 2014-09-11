#' setup components
#' 
#' @param DAE

powerComponentsSetup <- function(){
    Bus$setup();
    Line$setup();
    Shunt$setup();
    PVgen$setup();
    Slack$setup();
    PQload$setup();
    PQgen$setup();
    PQgen$addgen();
    Supply$setup();
}