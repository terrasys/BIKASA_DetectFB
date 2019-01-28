setwd(file.path(W.DIR,OUT.DIR))
pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_ST-",as.character(unique(l.o[[j]]$GEOHERK_ST)),"_Cluster",c(".pdf"),sep=""),
    width=8,height=6)
par(mfrow=c(2,3))
boxplot(o$SL ~ MC.SL, 
        data=o,
        horizontal=TRUE,
        ylab="Cluster",
        xlab=expression(paste(italic(SL))),
        las=1)
boxplot(o[[V.K]] ~ MC.K, 
        data=o,
        horizontal=TRUE,
        ylab="Cluster",
        xlab=expression(paste(italic(K))),
        las=1)
boxplot(o[[V.LS]] ~ MC.LS, 
        data=o,
        horizontal=TRUE,
        ylab="Cluster",
        xlab=expression(paste(italic(LS))),
        las=1)
boxplot(o[[V.UL]] ~ MC.UL, 
        data=o,
        horizontal=TRUE,
        ylim=c(0,2000),
        ylab="Cluster",
        xlab=expression(paste(italic(UL))),
        las=1)
boxplot(o[[V.US]] ~ MC.US, 
        data=o,
        horizontal=TRUE,
        ylab="Cluster",
        xlab=expression(paste(italic(US))),
        las=1)
boxplot(o[[V.MBI]] ~ MC.MBI, 
        data=o,
        horizontal=TRUE,
        ylab="Cluster",
        xlab=expression(paste(italic(MBI))),
        las=1)
dev.off()


setwd(file.path(W.DIR,OUT.DIR))
groupColumns = c("MC.K","GEOHERK_ST")
dataColumns = c("AREA")
agg.bc = ddply(o, groupColumns, function(x) colSums(x[dataColumns]))
pdf(paste(substr(SHP, start=1, stop=(nchar(SHP)-4)),"_Flaechenanteil_K-Faktorcluster",c(".pdf"),sep=""),
    width=8,height=10)
print(
  barchart(factor(MC.K) ~ AREA*100/sum(AREA) | GEOHERK_ST, 
           data=agg.bc,
           origin = 0,
           xlab=paste("Fl?chenanteil (Gesamtfl?che =",round(sum(agg.bc$AREA)/10000,0),"ha)"),
           ylab="K-Faktor-Cluster")
)
dev.off()


names.v <- c("MBI1","MBI2","K","US","UL","LS","SL")
for(n in names.v){
  df.CI[paste("CL.CI.",n,sep="")] <- NA
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]<=22,
                                            0,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]>=23 & df.CI[paste("CI.",n,sep="")]<=31,
                                            1,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]>=32 & df.CI[paste("CI.",n,sep="")]<=44,
                                            2,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]>=45 & df.CI[paste("CI.",n,sep="")]<=63,
                                            3,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]>=64 & df.CI[paste("CI.",n,sep="")]<=80,
                                            4,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
  df.CI[paste("CL.CI.",n,sep="")] <- ifelse(df.CI[paste("CI.",n,sep="")]>80,
                                            5,
                                            df.CI[[paste("CL.CI.",n,sep="")]])
}
