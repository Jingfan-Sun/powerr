#' setup components
#' 

powerComponentsSetup <- function(){
    DAE <- Bus$setup(DAE);
    Line$setup(Bus);
    Shunt$setup(Bus);
    DAE <- PVgen$setup(Bus, DAE);
    DAE <- Slack$setup(Bus, PVgen, DAE);
    PQload$setup(Bus);
    PQgen$setup(Bus);
    PQgen <- PQgen$addgen(PQload, PQgen, Bus);
    Supply <- Supply$setup(Bus);
    
}