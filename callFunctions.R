#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
W.DIR <- "d:/Dropbox/_git/DetectFB/"
FUNC.DIR <- "_functions/"
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackage.R"))
source(file.path(W.DIR,FUNC.DIR,"fTaSA.R"))
source(file.path(W.DIR,FUNC.DIR,"fZonaSt.R"))
source(file.path(W.DIR,FUNC.DIR,"fCompIx.R"))
fTaSA(W.DIR,
     IN.DIR <- "_data/",
     DEM = "DEM20",
     OUT.DIR = "_result/",
     FB="FB_LSA_EPSG31468.shp")


fZonaSt(W.DIR,
        SHP.DIR = "_data/",
        SHP="ObjectShapes.shp",
        OUT.DIR ="_result/",
        RASTER.DIR=paste(OUT.DIR,"_parameter/",sep=""))


fSiCOM(W.DIR,
        IN.DIR="_result/",
        SHP="ObjectShapes_RA.shp",
        OUT.DIR="_result/",
        V.CN=5,
        V.CI=80,
        V.GMK=2000,
        V.MBI="RA7",
        V.SLP="RA5",
        V.K="RA1",
        V.US="RA8",
        V.UL="RA9",
        V.LS="RA10")