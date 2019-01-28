print("Function: Comparison Index")
#-------------------------------------------------------------------------------
fCompIx <- function(W.DIR,
                    IN.DIR,
                    SHP,
                    V.CN,
                             V.GMK,
                             OUT.DIR,
                             V.MBI,
                             V.SLP,
                             V.K,
                             V.S,
                             V.L,
                             V.LS,
                             K.CLASS=TRUE)
  
#-------------------------------------------------------------------------------
print("Import shape file")  
#-------------------------------------------------------------------------------
o <- st_read(file.path(W.DIR,IN.DIR,SHP))
o$AREA <- st_area(o)
o <- o[which(o$GEOHERK_ST!='1.#INF'),]
head(o)
o$SL <- o[[paste(V.K)]]*o[[paste(V.LS)]]
head(o)
#-------------------------------------------------------------------------------
#print("Releation between  regions and terrain classes")
##-------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
groupColumns = c("TYPE_GMK","GEOHERK_ST")
dataColumns = c("AREA")
agg.bc = ddply(o, groupColumns, function(x) colSums(x[dataColumns]))
pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_Flaechenanteil_Standorttypen-GMK",c(".pdf"),sep=""),
    width=8,height=10)
print(
  barchart(factor(TYPE_GMK) ~ AREA*100/sum(AREA) | GEOHERK_ST, 
           data=agg.bc,
           origin = 0,
           xlab=paste("Flächenanteil (Gesamtfläche =",round(sum(agg.bc$AREA)/10000,0),"ha)"),
           ylab="GMK-Klassen")
)
dev.off()


#-------------------------------------------------------------------------------
print("Selection of areas, which are not floodplains")
#-------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
o <- o[which(o$TYPE_GMK>V.GMK),]

setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_Flaechenanteil_Standorttypen_GMK",V.GMK,c(".pdf"),sep=""),
    width=5,height=5)
agg <-aggregate(o$AREA, by=list(o$GEOHERK_ST),FUN=sum, na.rm=TRUE)
agg
sum.agg <- sum(agg[[2]])
print("Boxplots")
barchart(Group.1 ~ x*100/sum.agg,  data = as.data.frame(agg),
         main = "",
         xlab = paste('Flächenanteil [%] (Gesamtfläche =',round(sum.agg/10000,0),"ha)"),
         ylab = "Standorttyp",
         origin = 0)

dev.off()


#-------------------------------------------------------------------------------
print("Analysis of soil regions")
#-------------------------------------------------------------------------------
###K factor classification according to AG BOden (2005:p.366)
o$MC.K <- Mclust(model.matrix(~-1 + o[[c(paste(V.K,sep=""))]],o),G=V.CN)$classification
###MBI  classification
o$MC.MBI <- Mclust(model.matrix(~-1 + o[[c(paste(V.MBI,sep=""))]],o),G=V.CN)$classification
###US factor classification
o$MC.SLP <-  Mclust(model.matrix(~-1 + o[[c(paste(V.SLP,sep=""))]],o),G=V.CN)$classification
###Upper slope length classification
o$MC.UL <-  Mclust(model.matrix(~-1 + o[[c(paste(V.UL,sep=""))]],o),G=V.CN)$classification
###Upper slope classification
o$MC.US <-  Mclust(model.matrix(~-1 + o[[c(paste(V.US,sep=""))]],o),G=V.CN)$classification
###LS factor classification
o$MC.LS <-  Mclust(model.matrix(~-1 + o[[c(paste(V.LS,sep=""))]],o),G=V.CN)$classification
###Soil loss classification
o$MC.SL <-  Mclust(model.matrix(~-1 + o$SL,o),G=V.CN)$classification


#split according to soil regions
l.o <- split(o,o$GEOHERK_ST)
for(j in 2:length(l.o)){
#-------------------------------------------------------------------------------
print(paste("Calculation of Comparison Indizes and Dominance Triples:",names(l.o[j])))
#-------------------------------------------------------------------------------
#split data set according to FBID
o.fb <- split(l.o[[j]],l.o[[j]]$IDFB)
df.columnnames <- c("IDFB","AreaFB","CI.MBI1","DT.MBI1","CI.MBI2","DT.MBI2","CI.K","DT.K","CI.US","DT.US","CI.UL","DT.UL","CI.LS","DT.LS","CI.SL","DT.SL")
#calculating and plotting cluster-specific area proportions and CIs
df.CI=matrix(nrow=length(o.fb),ncol=length(df.columnnames))
df.CI=data.frame(df.CI)
colnames(df.CI) <- df.columnnames
pb <- txtProgressBar(min=1, max=length(o.fb), style=3)
for(i in 1:length(o.fb)){
  #Field bock ID
  df.CI[i,1] <- unique(o.fb[[i]]$IDFB)
  #Field block area
  df.CI[i,2] <- sum(o.fb[[i]]$AREA)
  ###Comparison indices
  ##MBI1
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.MBI),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.MBI=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.MBI1 <- (agg.tbl$MC.MBI*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,3] <- round(sum(agg.tbl$CI.MBI1),0)
  #Dominance triple
  df.CI[i,4] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##MBI2
  agg.tbl$MC.MBI = (V.CN+1)-agg.tbl$MC.MBI
  #agg.tbl$MC.MBI[agg.tbl$MC.MBI==(CN+1)] <- 0
  agg.tbl$CI.MBI2 <- (agg.tbl$MC.MBI*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,5] <- round(sum(agg.tbl$CI.MBI2),0)
  df.CI[i,6] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##K
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.K),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.K=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.K <- (agg.tbl$MC.K*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,7] <- round(sum(agg.tbl$CI.K),0)
  df.CI[i,8] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##US
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.US),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.US=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.US <- (agg.tbl$MC.US*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,9] <- round(sum(agg.tbl$CI.US),0)
  #Dominance triple
  df.CI[i,10] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##UL
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.UL),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.UL=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.UL <- (agg.tbl$MC.UL*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,11] <- round(sum(agg.tbl$CI.UL),0)
  #Dominance triple
  df.CI[i,12] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##LS
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.LS),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.LS=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.LS <- (agg.tbl$MC.LS*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,13] <- round(sum(agg.tbl$CI.LS),0)
  #Dominance triple
  df.CI[i,14] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  ##SL
  agg <-aggregate(o.fb[[i]]$AREA, by=list(o.fb[[i]]$MC.SL),FUN=sum, na.rm=TRUE)
  agg.tbl <-  data.frame(MC.SL=agg[[1]],
                         AREA.PP=agg[[2]]*100/sum(agg[[2]]))
  agg.tbl$CI.SL <- (agg.tbl$MC.SL*agg.tbl$AREA.PP)/(V.CN+1)
  df.CI[i,15] <- round(sum(agg.tbl$CI.SL),0)
  #Dominance triple
  df.CI[i,16] <- as.character(paste(agg.tbl[order(agg.tbl$AREA.PP,decreasing = TRUE),][1:3,1],sep="",collapse = ""))
  setTxtProgressBar(pb, i)
}
#-------------------------------------------------------------------------------
print("Analysis of CI values")
#-------------------------------------------------------------------------------
  if(df.CI$CI.SL>=80){
   df.CI <- df.CI[which(df.CI$CI.SL>=80),]
    #rpart
    fit <- rpart(CI.SL ~ CI.K + CI.MBI1 + CI.MBI2 + CI.UL + CI.US,
                  data = df.CI, parms = list(split = 'gini'))
    setwd(file.path(W.DIR,OUT.DIR))
    pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_CLCI",as.character(unique(l.CL.CI[[k]]$CL.CI.SL)),"_rpart",c(".pdf"),sep=""),
        width=8,height=8)
    plot(fit)
    text(fit)
    dev.off()
    #interaction
    l.CL.CI[[k]]$CL.CI <- interaction(as.factor(l.CL.CI[[k]]$CL.CI.US), 
                               as.factor(l.CL.CI[[k]]$CL.CI.UL),
                               as.factor(l.CL.CI[[k]]$CL.CI.MBI1),
                               as.factor(l.CL.CI[[k]]$CL.CI.MBI2),
                               sep="")
    ##plot proportions and number of interacted classes
    Count <- as.data.frame(as.matrix(table(l.CL.CI[[k]]$CL.CI)))
    Count$ClusterIA <- rownames(Count)
    Count[Count==0]<-NA
    Count <- na.omit(Count)
    l.CL.CI[[k]]$Count <- 1
    Area  <-as.data.frame(aggregate(l.CL.CI[[k]]$AreaFB, by=list(l.CL.CI[[k]]$CL.CI),FUN=sum, na.rm=TRUE))
    Count <- as.data.frame(aggregate(l.CL.CI[[k]]$Count, by=list(l.CL.CI[[k]]$CL.CI),FUN=sum, na.rm=TRUE))
    df.CI.agg <- data.frame(ClusterIA=Area$Group.1,Area=Area$x,Count=Count$x)
    groupColumns = c("ClusterIA")
    dataColumns = c("Area")
    agg.bc = ddply(df.CI.agg, groupColumns, function(x) colSums(x[dataColumns]))
    
    pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_CLCI",as.character(unique(l.CL.CI[[k]]$CL.CI.SL)),"_barchart-area",c(".pdf"),sep=""),
        width=6,height=6)
    print(
    barchart(factor(ClusterIA) ~ Area*100/sum(Area), 
           data=agg.bc,
           origin = 0,
           xlab=paste('Fl?chenanteil (Gesamtfläche =',round(sum(agg.bc$Area)/10000,0),"ha)"),
           ylab=paste("Reliefklassen der VI-Klasse",as.character(unique(l.CL.CI[[k]]$CL.CI.SL))))
   )
   dev.off()
   #plot
   groupColumns = c("ClusterIA")
   dataColumns = c("Count")
   agg.bc = ddply(df.CI.agg, groupColumns, function(x) colSums(x[dataColumns]))
   pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_CLCI",as.character(unique(l.CL.CI[[k]]$CL.CI.SL)),"_barchart-number",c(".pdf"),sep=""),
       width=8,height=8)
   print(
   barchart(factor(ClusterIA) ~ Count*100/sum(Count), 
           data=agg.bc,
           origin = 0,
           xlab=paste("Anzahl (Gesamt = ",round(sum(agg.bc$Count),0),")",sep=""),
           ylab="Relief- und Bodenabtragsklassen")
   )
  dev.off()
  ##export
  write.csv2(df.CI,
             paste(W.DIR,OUT.DIR,substr(SHP, start=1, stop=(nchar(SHP)-4)),"_",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_Feldblockbewertung",c(".csv"),sep=""))
  o <- merge(o,df.CI,by="IDFB")
  o$AREA <- st_area(o)
  setwd(file.path(W.DIR,OUT.DIR))
  st_write(o,
           paste(W.DIR,OUT.DIR,substr(SHP, start=1, stop=(nchar(SHP)-4)),"_",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_Feldblockbewertung",c(".shp"),sep=""),
           delete_layer = TRUE)

  }
  }

#####test











##field block-specific boxplots
l.CL.CI <- split(o,o$CL.CI)
for(i in (1:length(l.CL.CI))){
  if(nrow(l.CL.CI[[i]])>0){
    
    
    l.CL.CI.FB <- split(l.CL.CI$`424`,l.CL.CI$`424`$IDFB)
    #l.CL.CI.FB <- split(l.CL.CI[[i]],l.CL.CI[[i]]$IDFB)
    setwd(file.path(W.DIR,OUT.DIR))
    pdf(paste(W.DIR,OUT.DIR,"FeldblockBewertung_CI",as.character(unique(l.CL.CI.FB[[i]]$CL.CI)),"__",substr(SHP, start=1, stop=(nchar(SHP)-4)),c(".pdf"),sep=""),
        width=7,height=3)
    for(j in (1:length(l.CL.CI.FB))){
      par(mfrow=c(1,3))
      #area bar plot for all crop types
      agg <-aggregate(l.CL.CI.FB[[j]]$AREA, by=list(l.CL.CI.FB[[j]]$MC.MBI),FUN=sum, na.rm=TRUE)
      sum.agg <- sum(agg[[2]])
      t.sum <- tapply(agg[[2]],agg[[1]],sum)
      xx <- barplot(t.sum*100/sum.agg,
                    ylab="Fl?chenanteil [%]",
                    sub=paste('Gesamtfl?che =',round(sum.agg/10000,1),' ha'),
                    xlab=expression(paste(italic(MBI),"-Cluster")),
                    las=1,
                    ylim=c(0,100),
                    main=paste(unique(l.CL.CI.FB[[j]]$FBID)))
      text(x = xx, y = t.sum*100/sum.agg, label = round(t.sum*100/sum.agg,1), pos = 3, cex = 0.8, col = "red")
      legend("topleft", 
             legend=c(as.expression(bquote({italic(CI)^{MBI}^{1}} == .(round(unique(l.CL.CI.FB[[j]]$CI.MBI1),0)))),
                      as.expression(bquote({italic(CI)^{MBI}^{2}} == .(round(unique(l.CL.CI.FB[[j]]$CI.MBI2),0))))),
             bty="n",cex=1)
      
      
      agg <-aggregate(l.CL.CI.FB[[j]]$AREA, by=list(l.CL.CI.FB[[j]]$MC.SL),FUN=sum, na.rm=TRUE)
      sum.agg <- sum(agg[[2]])
      t.sum <- tapply(agg[[2]],agg[[1]],sum)
      xx <- barplot(t.sum*100/sum.agg,
                    ylab="Fl?chenanteil [%]",
                    xlab=expression(paste(italic(A),"-Cluster")),
                    las=1,
                    ylim=c(0,100),
                    main="")
      text(x = xx, y = t.sum*100/sum.agg, label = round(t.sum*100/sum.agg,1), pos = 3, cex = 0.8, col = "red")
      legend("topleft", 
             legend=c(as.expression(bquote({italic(CI)^{A}} == .(round(unique(l.CL.CI.FB[[j]]$CI.A)))))),
             bty="n",cex=1)
      
      
      agg <-aggregate(l.CL.CI.FB[[j]]$AREA, by=list(l.CL.CI.FB[[j]]$MC.VDC),FUN=sum, na.rm=TRUE)
      sum.agg <- sum(agg[[2]])
      t.sum <- tapply(agg[[2]],agg[[1]],sum)
      xx <- barplot(t.sum*100/sum.agg,
                    ylab="Fl?chenanteil [%]",
                    xlab=expression(paste(italic(VDC),"-Cluster")),
                    las=1,
                    ylim=c(0,100),
                    main="")
      text(x = xx, y = t.sum*100/sum.agg, label = round(t.sum*100/sum.agg,1), pos = 3, cex = 0.8, col = "red")
      legend("topleft", 
             legend=c(as.expression(bquote({italic(CI)^{VDC}} == .(round(unique(l.CL.CI.FB[[j]]$CI.VDC)))))),
             bty="n",cex=1)
    }
    dev.off()
  }
} 



