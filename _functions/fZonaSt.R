print("Function: Zonal statistics of reference units for raster data")
#-------------------------------------------------------------------------------
fZonaSt <- function(
  W.DIR,
  SHP.DIR,
  SHP,
  RASTER.DIR,
  OUT.DIR){
#-----------------------------------------------------------------------------------------------------
print("Import reference units")
#-----------------------------------------------------------------------------------------------------
o <- st_read(file.path(W.DIR,SHP.DIR,SHP))
#select polygones within field blocks
#o.fb <- o[o$FBID != "1.#INF",]
o.fb <- o[o$BNFB == "AL",]
#export field blochk shape
setwd(file.path(W.DIR,OUT.DIR))
st_write(o.fb,
         paste(substr(SHP,1,nchar(SHP)-4),"_RA.shp",sep=""),
         delete_layer = TRUE)
#-----------------------------------------------------------------------------------------------------
print("Import terrain attributes")
#-----------------------------------------------------------------------------------------------------
setwd(file.path(W.DIR,RASTER.DIR))
l.r <- mixedsort(list.files(pattern=paste(".*\\.asc$",sep="")),decreasing=TRUE)
#create a layerstack
m <- stack(l.r)
#-----------------------------------------------------------------------------------------------------
print("Transform imagery in SAGA format")
#-----------------------------------------------------------------------------------------------------
pb <- txtProgressBar(min=0, max=length(m@layers), style=3)  
for(i in 1:length(l.r)){
  writeRaster(m[[i]],
              paste(substr(l.r[i],0,nchar(l.r[i])-4),c(".sgrd"),sep=""),
              format="SAGA",
              overwrite=TRUE)
  setTxtProgressBar(pb, i)
}
#-----------------------------------------------------------------------------------------------------
print("Zonal statistic")
#-----------------------------------------------------------------------------------------------------
setwd(file.path(W.DIR,RASTER.DIR))
l.r <- mixedsort(list.files(pattern=paste(".*\\.sgrd$",sep="")),decreasing=TRUE)
pb <- txtProgressBar(min=0, max=length(l.r), style=3)
for (i in 1:length(l.r)){
  #import
    rsaga.geoprocessor(
    lib="shapes_grid",
    module=2,
    param=list(GRIDS=file.path(W.DIR,RASTER.DIR,l.r[i]),
               POLYGONS=paste(W.DIR,OUT.DIR,substr(SHP,1,nchar(SHP)-4),"_RA.shp",sep=""),
               COUNT=0,
               MIN=0,
               MEAN=0,
               MAX=1,
               RANGE=0,
               SUM=0,
               VAR=0,
               STDDEV=0,
               QUANTILE=0,
               NAMING=0),
    env=myenv)
  setTxtProgressBar(pb, i)
}
print("Import and rename shape file attributes")
setwd(file.path(W.DIR,OUT.DIR))
o <- st_read(paste(substr(SHP,1,nchar(SHP)-4),"_RA.shp",sep=""))
colnames(o) <- c(names(o.fb)[-length(o.fb)],paste("RA",1:length(l.r),sep=""),paste("geometry"))
write.csv2(data.frame(CODE=paste("RA",1:length(l.r),sep=""),NAME=l.r),
            "readme_terrainattributes.txt",
                      row.names = FALSE)

#Filtering according to specific values 
l.ra <- paste("RA",1:length(l.r),sep="")
pb <- txtProgressBar(min=0, max=length(l.ra), style=3)
for(i in (1:length(l.ra))){
  o <- o[o[[l.ra[i]]] > -2,]
  setTxtProgressBar(pb, i)
  }
nrow(o)
st_write(o,
         paste(W.DIR,OUT.DIR,substr(SHP,1,nchar(SHP)-4),"_RA.shp",sep=""),
         delete_layer = TRUE)
head(o)
}
