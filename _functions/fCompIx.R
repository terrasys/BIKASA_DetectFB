print("Function: Comparison Index")
#-------------------------------------------------------------------------------
fComparIx <- function(W.DIR,
                    IN.DIR,
                    RO.SHP,
                    FB.SHP,
                    OUT.DIR,
                    V.CN,
                    V.OL,
                    V.GMK,
                    V.MBI,
                    V.SLP,
                    V.K,
                    V.US,
                    V.UL,
                    V.LS,
                    V.TH){
#-------------------------------------------------------------------------------
print("Import shape file")  
#-------------------------------------------------------------------------------
o <- st_read(file.path(W.DIR,IN.DIR,RO.SHP))
o$AREA <- st_area(o)
o <- o[which(o$GEOHERK_ST!='1.#INF'),]
nrow(o)
o$SL <- o[[paste(V.K)]]*o[[paste(V.LS)]]
#-------------------------------------------------------------------------------
print("Calculation of field block areas")
#-------------------------------------------------------------------------------
o$AREA <- st_area(o)
groupColumns = c("FBID")
dataColumns = c("AREA")
agg.fb = ddply(o, groupColumns, function(x) colSums(x[dataColumns]))
colnames(agg.fb) <- c("FBID","AREA.FB")
o <- merge(o,agg.fb,by="FBID")
#-------------------------------------------------------------------------------
print("Calculation of field block and ST areas")
#-------------------------------------------------------------------------------
o$FBIDST <- interaction(o$FBID,o$GEOHERK_ST,sep="",drop = TRUE)
groupColumns = c("FBIDST")
dataColumns = c("AREA")
agg.fbst = ddply(o, groupColumns, function(x) colSums(x[dataColumns]))
colnames(agg.fbst) <- c("FBIDST","AREA.FBST")
o <- merge(o,agg.fbst,by="FBIDST")
#-------------------------------------------------------------------------------
#print("Releation between  regions and terrain classes")
##-------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
groupColumns = c("TYPE_GMK","GEOHERK_ST")
dataColumns = c("AREA")
agg.bc = ddply(o, groupColumns, function(x) colSums(x[dataColumns]))
pdf(paste(substr(RO.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Flaechenanteil_Standorttypen-GMK",c(".pdf"),sep=""),
    width=10,height=7)
print(
  barchart(factor(TYPE_GMK) ~ AREA*100/sum(AREA) | GEOHERK_ST, 
           data=agg.bc,
           origin = 0,
           xlab=paste("Flächenanteile 'Geologische Entstehung' [%] (Gesamtfläche =",round(sum(agg.bc$AREA)/10000,0),"ha)"),
           ylab="GMK-Klassen")
)
dev.off()
#-------------------------------------------------------------------------------
print("Selection of areas, which are not floodplains")
#-------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
o <- o[which(o$TYPE_GMK>V.GMK),]
nrow(o)

setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(substr(RO.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Flaechenanteil_Standorttypen_GMK",V.GMK,c(".pdf"),sep=""),
    width=5,height=5)
agg <-aggregate(o$AREA, by=list(o$GEOHERK_ST),FUN=sum, na.rm=TRUE)
agg
sum.agg <- sum(agg[[2]])
print("Boxplots")
barchart(Group.1 ~ x*100/sum.agg,  data = as.data.frame(agg),
         main = "",
         xlab=paste("Flächenanteile [%] (Gesamtfläche =",round(sum.agg/10000,0),"ha)"),
         ylab = "Geologische Entstehung",
         origin = 0)

dev.off()
#-------------------------------------------------------------------------------
print("Cluster analysis")
#-------------------------------------------------------------------------------
###K factor
o$MC.K <- Mclust(model.matrix(~-1 + o[[c(paste(V.K,sep=""))]],o),G=V.CN)$classification
###MBI  
o$MC.MBI <- Mclust(model.matrix(~-1 + o[[c(paste(V.MBI,sep=""))]],o),G=V.CN)$classification
###US 
o$MC.SLP <-  Mclust(model.matrix(~-1 + o[[c(paste(V.SLP,sep=""))]],o),G=V.CN)$classification
###Upper slope length
o$MC.UL <-  Mclust(model.matrix(~-1 + o[[c(paste(V.UL,sep=""))]],o),G=V.CN)$classification
###Upper slope 
o$MC.US <-  Mclust(model.matrix(~-1 + o[[c(paste(V.US,sep=""))]],o),G=V.CN)$classification
###LS factor 
o$MC.LS <-  Mclust(model.matrix(~-1 + o[[c(paste(V.LS,sep=""))]],o),G=V.CN)$classification
###Soil loss 
o$MC.SL <-  Mclust(model.matrix(~-1 + o$SL,o),G=V.CN)$classification
setwd(file.path(W.DIR,OUT.DIR))
st_write(o,
         paste(W.DIR,OUT.DIR,substr(RO.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Cluster",c(".shp"),sep=""),
         delete_layer = TRUE)


#Plot cluster results
setwd(file.path(W.DIR,OUT.DIR))
#pdf(paste(substr(RO.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Cluster",c(".pdf"),sep=""),
#    width=12,height=5)
png(paste(substr(RO.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Cluster",c(".png"),sep=""),width=2200,height=1200,res=200)
par(mfrow=c(2,6))
boxplot(o$SL ~ MC.SL, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,1),
        ylab="Cluster",
        xlab=expression(paste(italic(Enat))),
        las=1)
boxplot(o[[c(paste(V.K,sep=""))]] ~ MC.K, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,1),
        ylab="Cluster",
        xlab=expression(paste(italic(K))),
        las=1)
boxplot(o[[c(paste(V.LS,sep=""))]] ~ MC.LS, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,2),
        ylab="Cluster",
        xlab=expression(paste(italic(LS))),
        las=1)
boxplot(o[[c(paste(V.UL,sep=""))]] ~ MC.UL, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,2000),
        ylab="Cluster",
        xlab=expression(paste(italic(UL))),
        las=1)
boxplot(o[[c(paste(V.US,sep=""))]] ~ MC.US, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,0.2),
        ylab="Cluster",
        xlab=expression(paste(italic(US))),
        las=1)

boxplot(o[[c(paste(V.MBI,sep=""))]] ~ MC.MBI, 
        data=o,
        horizontal=TRUE,
        ylim=c(-1,2),
        ylab="Cluster",
        xlab=expression(paste(italic(MBI))),
        las=1)

plot(density(o$SL),
     xlim=c(0,1),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(Enat))))
plot(density(o[[c(paste(V.K,sep=""))]]),
     xlim=c(0,1),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(K))))
plot(density(o[[c(paste(V.LS,sep=""))]]),
     xlim=c(0,2),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(LS))))
plot(density(o[[c(paste(V.UL,sep=""))]]),
     xlim=c(0,2000),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(UL))))
plot(density(o[[c(paste(V.US,sep=""))]]),
     xlim=c(0,0.2),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(US))))

plot(density(o[[c(paste(V.MBI,sep=""))]]),
     xlim=c(-1,2),
     main="",
     las=1,
     ylab="Dichtefunktion",
     xlab=expression(paste(italic(MBI))))
dev.off()


#-------------------------------------------------------------------------------
print("Calculation of Comparison Indizes and Dominance Triples for each single field block")
#-------------------------------------------------------------------------------
head(o)
#split data set according to FBID
o.fb <- split(o,o$FBIDST,drop = TRUE)
df.columnnames <- c("IDFB","AreaFB","AreaFBST","CI.MBI1","DT.MBI1","CI.MBI2","DT.MBI2","CI.K","DT.K","CI.US","DT.US","CI.UL","DT.UL","CI.LS","DT.LS","CI.SL","DT.SL","ST","FBID")
#calculating and plotting cluster-specific area proportions and CIs
df.CI=matrix(nrow=length(o.fb),ncol=length(df.columnnames))
df.CI=data.frame(df.CI)
colnames(df.CI) <- df.columnnames
pb <- txtProgressBar(min=1, max=length(o.fb), style=3)
for(i in 1:length(o.fb)){
  #Field bock ID
  df.CI[i,1] <- unique(o.fb[[i]]$IDFB)
  #Field block area
  df.CI[i,2] <- unique(o.fb[[i]]$AREA.FB)
  #Field block and ST area
  df.CI[i,3] <- unique(o.fb[[i]]$AREA.FBST)
    ###Comparison indices
  ##MBI1
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.MBI),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.MBI=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.MBI1 <- (agg.tbl$MC.MBI*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,4] <- round(sum(agg.tbl$CI.MBI1),0)
  #Dominance triple
  df.CI[i,5] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##MBI2
  agg.tbl$MC.MBI = (V.CN+1)-agg.tbl$MC.MBI
  #agg.tbl$MC.MBI[agg.tbl$MC.MBI==(CN+1)] <- 0
  agg.tbl$CI.MBI2 <- (agg.tbl$MC.MBI*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,6] <- round(sum(agg.tbl$CI.MBI2),0)
  df.CI[i,7] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##K
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.K),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.K=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.K <- (agg.tbl$MC.K*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,8] <- round(sum(agg.tbl$CI.K),0)
  df.CI[i,9] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##US
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.US),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.US=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.US <- (agg.tbl$MC.US*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,10] <- round(sum(agg.tbl$CI.US),0)
  #Dominance triple
  df.CI[i,11] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##UL
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.UL),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.UL=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.UL <- (agg.tbl$MC.UL*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,12] <- round(sum(agg.tbl$CI.UL),0)
  #Dominance triple
  df.CI[i,13] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##LS
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.LS),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.LS=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.LS <- (agg.tbl$MC.LS*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,14] <- round(sum(agg.tbl$CI.LS),0)
  #Dominance triple
  df.CI[i,15] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##SL
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.SL),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.SL=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.SL <- (agg.tbl$MC.SL*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,16] <- round(sum(agg.tbl$CI.SL),0)
  #Dominance triple
  df.CI[i,17] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  setTxtProgressBar(pb, i)
  #Geoherkunft
  df.CI[i,18] <- as.character(unique(o.fb[[i]]$GEOHERK_ST))
  #FBID
  df.CI[i,19] <- as.character(unique(o.fb[[i]]$FBID))
  setTxtProgressBar(pb, i)
}
#CI (Enat) cluster analysis
#df.CI$MC.CI.SL <- Mclust(model.matrix(~-1 + CI.SL,df.CI))$classification
#Merging of cluster analysis result with field block file
fb <- st_read(file.path(W.DIR,IN.DIR,FB.SHP))
head(df.CI)
fb <- merge(fb,df.CI,by="FBID")
##Boxplot of 
#setwd(file.path(W.DIR,OUT.DIR))
#pdf(paste(substr(FB.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Cluster_CI-SL",c(".pdf"),sep=""),
#    width=4,height=8)
#par(mfrow=c(2,1))
#boxplot(CI.SL ~ MC.CI.SL, 
#        data=df.CI,
#        horizontal=TRUE,
#        ylim=c(0,100),
#        ylab="Cluster",
#        xlab=expression(paste(italic(CI^Enat))),
#        las=1)
#plot(density(df.CI$CI.SL),
#     xlim=c(0,100),
#     main="",
#     las=1,
#     ylab="Dichtefunktion",
#     xlab=expression(paste(italic(CI^Enat))))
#dev.off()

setwd(file.path(W.DIR,OUT.DIR))
st_write(fb,
         paste(W.DIR,OUT.DIR,substr(FB.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_Feldblockbewertung",c(".shp"),sep=""),
         delete_layer = TRUE)

#head(df.CI)
#-------------------------------------------------------------------------------
print("Analysis of CI values")
#-------------------------------------------------------------------------------
#nrow(df.CI)
#selection of parcels which belong to specific CI.SL cluster  
head(fb)
df.CI.E <- fb[which(fb$CI.SL>=V.TH),]
nrow(df.CI.E)

#selection of parcels with a certain overlap  
df.CI.E <- df.CI.E[which((df.CI.E$AreaFBST*100/df.CI.E$AreaFB)>=V.OL),]
nrow(df.CI.E)
setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(substr(FB.SHP, start=1, stop=(nchar(RO.SHP)-4)),"_CI_Flaechenanteil_Standorttypen_Erosion_TH",V.TH,c(".pdf"),sep=""),
    width=5,height=5)
agg <-aggregate(df.CI.E$AreaFBST, by=list(df.CI.E$ST),FUN=sum, na.rm=TRUE)
agg
sum.agg <- sum(agg[[2]])
print("Boxplots")
barchart(Group.1 ~ x*100/sum.agg,  data = as.data.frame(agg),
         main = "",
         xlab = paste('Flächenanteil [%] (Gesamtfläche =',round(sum.agg/10000,0),"ha)"),
         ylab = "Geologische Entstehung",
         origin = 0)

dev.off()


  #Random Foest
  set.seed(123)
  ctrl <- trainControl(method="repeatedcv",number=5)
  rf.Fit <-   train(CI.SL ~ CI.K + CI.MBI1 + CI.MBI2 + CI.UL + CI.US + ST,
                    data = df.CI.E,
                    ntree=1000,
                    method = "rf",
                    trControl = ctrl,
                    preProc = c("center", "scale"),
                    importance = TRUE,
                    verbose = TRUE,
                    varImp.train=FALSE)
  
  
  setwd(file.path(W.DIR,OUT.DIR))
  sink(paste(substr(FB.SHP, start=1, stop=(nchar(FB.SHP)-4)),"_CI_Erosion_rf","_TH",V.TH,c(".txt"),sep=""))
  print(rf.Fit)
  print(varImp(rf.Fit,scale=TRUE))
  sink()

  
  
  #split according to soil regions
  df.CI.ST <- split(df.CI.E,df.CI.E$ST,drop = TRUE)
  names(df.CI.ST)
  for(k in 1:length(df.CI.ST)){
    if(nrow(df.CI.ST[[k]])>10){
    #Random Forest
    set.seed(123)
    ctrl <- trainControl(method="repeatedcv",number=5)
    rf.Fit <-   train(CI.SL ~ CI.K + CI.MBI1 + CI.MBI2 + CI.UL + CI.US,
                      data = df.CI.ST[[k]],
                      ntree=1000,
                      method = "rf",
                      trControl = ctrl,
                      preProc = c("center", "scale"),
                      importance = TRUE,
                      verbose = TRUE,
                      varImp.train=FALSE)
    
    setwd(file.path(W.DIR,OUT.DIR))
    sink(paste(substr(FB.SHP, start=1, stop=(nchar(FB.SHP)-4)),"_VI","_",as.character(unique(df.CI.ST[[k]]$ST)),"_TH",V.TH,c(".txt"),sep=""))
    print(rf.Fit)
    print(varImp(rf.Fit,scale=TRUE))
    sink()
    #interaction
    df.CI.ST[[k]]$I.CI <- interaction(as.factor(round(df.CI.ST[[k]]$CI.US/10,0)), 
                                 as.factor(round(df.CI.ST[[k]]$CI.UL/10,0)),
                                 as.factor(round(df.CI.ST[[k]]$CI.MBI1/10,0)),
                                 as.factor(round(df.CI.ST[[k]]$CI.MBI2/10,0)),
                                 sep="")
    head(df.CI.ST[[k]])
    ##plot proportions and number of interacted classes
    Count <- as.data.frame(as.matrix(table(df.CI.ST[[k]]$I.CI)))
    Count$ClusterIA <- rownames(Count)
    Count[Count==0]<-NA
    Count <- na.omit(Count)
    df.CI.ST[[k]]$Count <- 1
    Area  <-as.data.frame(aggregate(df.CI.ST[[k]]$AreaFB, by=list(df.CI.ST[[k]]$I.CI),FUN=sum, na.rm=TRUE))
    Count <- as.data.frame(aggregate(df.CI.ST[[k]]$Count, by=list(df.CI.ST[[k]]$I.CI),FUN=sum, na.rm=TRUE))
    df.CI.agg <- data.frame(ClusterIA=Area$Group.1,Area=Area$x,Count=Count$x)
    groupColumns = c("ClusterIA")
    dataColumns = c("Area")
    agg.bc = ddply(df.CI.agg, groupColumns, function(x) colSums(x[dataColumns]))
    #select best  
    agg.bc <- head(agg.bc[order(agg.bc$Area,decreasing = TRUE), ], 20)
    #plot
    pdf(paste(substr(FB.SHP, start=1, stop=(nchar(FB.SHP)-4)),"_Feldblockbewertung","_",as.character(unique(df.CI.ST[[k]]$ST)),"_TH",V.TH,c(".pdf"),sep=""),
        width=4,height=6)
    print(
      barchart(factor(ClusterIA) ~ Area*100/sum(Area), 
               data=agg.bc,
               origin = 0,
               xlab=paste('Flächenanteile (Gesamtfläche =',round(sum(agg.bc$Area)/10000,0),"ha)"),
               ylab="Reliefklassen"))
    dev.off()
        ##export
    setwd(file.path(W.DIR,OUT.DIR))
    st_write(df.CI.ST[[k]],
             paste(W.DIR,OUT.DIR,substr(FB.SHP, start=1, stop=(nchar(FB.SHP)-4)),"_Feldblockbewertung_",as.character(unique(df.CI.ST[[k]]$ST)),"_TH",V.TH,c(".shp"),sep=""),
             delete_layer = TRUE)
    
    }
  }
  }



