#' powerComponentSetup
#' 
#' Setup components in powerr
#' 

powerComponentsSetup <- function(){
    
    Bus = Line = Shunt = PVgen = Slack = PQload = PQgen = Supply = NULL;
    
    .GlobalEnv$Bus$setup();
    .GlobalEnv$Line$setup();
    .GlobalEnv$Shunt$setup();
    .GlobalEnv$PVgen$setup();
    .GlobalEnv$Slack$setup();
    .GlobalEnv$PQload$setup();
    .GlobalEnv$PQgen$setup();
    .GlobalEnv$PQgen$addgen();
    .GlobalEnv$Supply$setup();
}