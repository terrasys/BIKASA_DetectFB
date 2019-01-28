#########################################################################################################
## Required libraries
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("maptools",
              "gstat",
              "RSAGA",
              "raster",
              "stats",
              "lattice",
              "sf",
              "gtools",
              "utils",
              "rpart",
              "rgdal",
              "sp",
              "RColorBrewer",
              "classInt",
              "parallel",
              "snow",
              "mclust",
              "fpc",
              "caret",
              "C50",
              "tidyr"))
loadandinstall(packages)
#-----------------------------------------------------------------------------------------------------
#function for the creation of exponetial values
#-----------------------------------------------------------------------------------------------------
lseq <- function(from, to, length.out) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}
#-----------------------------------------------------------------------------------------------------
#rsaga environment
#-----------------------------------------------------------------------------------------------------
myenv <- rsaga.env(workspace=W.DIR, 
                   path="c:/_saga_222_x64",
                   modules = "c:/_saga_222_x64/modules")
#print("RSAGA modules")
#print(rsaga.get.libraries(path= myenv$modules))
#rsaga.get.lib.modules("ta_hydrology", env =  myenv, interactive = FALSE)
#rsaga.get.usage("ta_hydrology",25,env =  myenv)
