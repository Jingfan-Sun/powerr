#' setup components
#' 

powerComponentsSetup <- function(){
    DAE <- Bus$setup(DAE);
    Line$setup(Bus);
}