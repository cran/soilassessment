\name{PHharm_Info}
\alias{PHharm_Info}
\title{
Information on performance of soil pH (water) harmonization models
}
\description{
Information index for relative predictive performance of soil pH harmonization models
}
\usage{
PHharm_Info(solution)
}
\arguments{
  \item{solution}{
solution for measuring soil pH
}
}
\details{
Solution for measuring soil pH. It’s given in quotation marks. Current models consider “cacl2” and “kcl” ratios. Default solution is “cacl2”
}
\value{
Graphical display of the predictive performance index for the harmonization models in different regions of the world: Africa, Asia, Near East and North Africa (NENA), Latin America and Caribbean (LAC), north America, and Europe. The performance index ranges between 0 (poor) to 1 (best).
}
\author{
Christian Thine Omuto
}
\note{
The function currently works for cacl2 and kcl. These solutions must be entered in quotation marks.Due to periodic update,internet connectivity is needed for the function to work.
}
\seealso{
\code{\link{ECharm_Info}}, \code{\link{SASdata_densityInfo}}
}
\examples{
# Internet connectivity is required to run the function as PHharm_Info("kcl")
Index=data.frame(Kabala=c(0.866,0.846,0.844,0.765,0.791,0.168,0.701),
                 Sadovski=c(0.898,0.846,0.863,0.752,0.802,0.094,0.740),
                 Polynomial=c(0.861,0.815,0.820,0.769,0.778,0.166,0.698),
                 Power=c(0.875,0.807,0.825,0.733,0.765,0.112,0.715),
                 Sigmoid=c(0.850,0.814,0.815,0.760,0.778,0.142,0.700),
                 Linear=c(0.875,0.807,0.825,0.733,0.765,0.112,0.715),
                 Gaussian=c(0.848,0.815,0.815,0.762,0.779,0.148,0.701),
                 Spherical=c(0.848,0.814,0.818,0.762,0.779,0.142,0.703),
                 Exponential=c(0.863,0.813,0.818,0.753,0.774,0.108,0.713))
rownames(Index)=c("Africa","America","Asia","Europe","LAC","NENA","Pacific")
regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
Index=as.matrix(Index)
barplot(Index, las=2,beside = TRUE,xpd = FALSE,col=regioncol,ylim=c(0,1.2),
horiz = FALSE, cex.names = 0.75,space = c(0.4, 4))
legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,
legend = rownames(Index), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
box(lty = 1, col = 'black')
mtext(side=1, text="PH harmonization models", line=3.7)
mtext(side=2, text="Performance index", line=2.5)
}
