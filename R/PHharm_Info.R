PHharm_Info=function(solution="cacl2"){
  inputInfo=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vScFSEj-QubwWOJIHDcYVGY-zZ_bbIgJQlvvePCnXy8r-nY9kHm9IebYtIcXwKPKfLtxqp1y59A1AgY/pub?output=csv")
 if(solution=="kcl") {
  df1=as.matrix(inputInfo[,2:10])
  row.names(df1)=c("Africa","N.America","Asia","Europe","LAC","NENA","Pacific")
  regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
  par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
  barplot(df1, las=2,beside = TRUE,xpd = F,col=regioncol,ylim=c(0,1.2),horiz = F, cex.names = 0.75,space = c(0.4, 4))
  grid (nx=NA, ny=NULL, lty = 6, col = "gray")
  legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,legend = rownames(df1), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
  box(lty = 1, col = 'black')
  mtext(side=1, text="PH harmonization models", line=3.7)
  mtext(side=2, text="Performance index", line=2.5)}
  else if(solution=="cacl2") {
    inputInfo=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT-t0Ds8kMX8Wt1iceiPfseD_P3-q4mi1XVV3STS7gy9hL5pMen6SNJvO3Afd6yJMWitR_zjKPH9Mao/pub?output=csv")
    df1=as.matrix(inputInfo[,2:11])
    row.names(df1)=c("Africa","N.America","Asia","Europe","LAC","NENA","Pacific")
    regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
    par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
    barplot(df1, las=2,beside = TRUE,xpd = F,col=regioncol,ylim=c(0,1.2),horiz = F, cex.names = 0.75,space = c(0.4, 4))
    grid (nx=NA, ny=NULL, lty = 6, col = "gray")
    legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,legend = rownames(df1), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
    box(lty = 1, col = 'black')
    mtext(side=1, text="PH harmonization models", line=3.7)
    mtext(side=2, text="Performance index", line=2.5)}

}
