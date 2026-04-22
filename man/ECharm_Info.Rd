\name{ECharm_Info}
\alias{ECharm_Info}
\title{
Information on performance of soil electrical conductivity (EC) harmonization models
}
\description{
Information index for relative predictive performance of soil EC harmonization models
}
\usage{
ECharm_Info(solution)
}
\arguments{
  \item{solution}{
ratio of soil-water solution for the extract used in measuring electrical conductivity.
}
}
\details{
Ratio in text format for the soil-water solution for the extract used in measuring EC. It’s given in quotation marks. Current models consider “1:2”, “1:2.5”, and “1:5” ratios. Default ratio is “1:2”
}
\value{
Graphical display of the predictive performance index for the harmonization models in different regions of the world: Africa, Asia, Near East and North Africa (NENA), Latin America and Caribbean (LAC), north America, and Europe. The performance index ranges between 0 (poor) to 1 (best).
}
\author{
Christian Thine Omuto
}
\note{
The function currently works for 1:2, 1:2.5, and 1:5. These ratios must be entered in quotation marks.Due to periodic update,internet connectivity is needed for the function to work.
}
\seealso{
\code{\link{PHharm_Info}}, \code{\link{SASdata_densityInfo}}
}
\examples{
# Internet connectivity is required to run the function as ECharm_Info("1:2")
Index=data.frame(Africa=c(-0.49,-0.39,-0.24,0.77,0.6,0.75,0.6,0.73,0.79,0.37),
                 America=c(0.48,-0.33,-0.22,0.82,0.71,0.7,0.67,0.63,0.7,0.49),
                 Asia=c(0.23,-0.33,0.12,0.88,0.81,0.45,0.78,0.55,0.44,0.61),
                 Europe=c(0.34,-0.21,-0.12,0.84,0.34,0.01,0.55,0.71,0.08,0.77),
                 LAC=c(0.45,-0.81,0.59,0.05,0.38,-0.13,0.38,-0.14,0.08,0.19),
                NENA=c(0.51,0.53,-0.21,0.88,0.77,0.23,0.56,0.77,0.75,0.54),
                 Pacific=c(0.54,0.57,0.44,0.78,0.74,0.82,0.74,0.79,0.78,0.73))
rownames(Index)=c("FAO","Sonmez","Ozcan","Polynomial","Power","Sigmoid","Linear",
"Gaussian","Spherical","Exponential")
regioncol=c("blue","red","cyan","magenta","green","gray", "yellow","brown",
"darkolivegreen","orange")
par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
Index=as.matrix(Index)
barplot(Index, las=2,beside = TRUE,xpd = FALSE,col=regioncol,ylim=c(0,1.2),
horiz = FALSE, cex.names = 0.75,space = c(0.4, 4))
legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,
legend = rownames(Index), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
box(lty = 1, col = 'black')
mtext(side=1, text="Regions of the World", line=3.7)
mtext(side=2, text="Performance index", line=2.5)
}
