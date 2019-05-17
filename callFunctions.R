#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
W.DIR <- "d:/Dropbox/_git/BIKASA_DetectFB/"
FUNC.DIR <- "_functions/"
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackage.R"))
source(file.path(W.DIR,FUNC.DIR,"fTaSA.R"))
source(file.path(W.DIR,FUNC.DIR,"fZonaSt.R"))
source(file.path(W.DIR,FUNC.DIR,"fComparIx.R"))
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


fComparIx(W.DIR,
        IN.DIR="_data/", 
        RO.SHP="ObjectShapes_RA.shp",
        FB.SHP="FB_LSA_EPSG31468.shp",
        OUT.DIR="_result/",
        V.CN=6,
        V.GMK=2000,
        V.MBI="RA7",
        V.SLP="RA5",
        V.K="RA1",
        V.US="RA8",
        V.UL="RA9",
        V.LS="RA10",
        V.TH=8)

