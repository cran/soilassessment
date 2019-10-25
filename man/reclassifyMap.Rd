\name{reclassifyMap}
\alias{reclassifyMap}
\title{
A function for re-classifying raster maps based on input look-up table
}
\description{
This function re-classifies an input raster maps based on input look-up table that specifies transition from map classes (or range of classes) to a new class (or range of classes)
}
\usage{
reclassifyMap(fgrid,df)
}
\arguments{
  \item{fgrid}{
Input raster map to be reclassified
}
\item{df}{
Input look-up table for re-classification
}
}
\details{
The look-up table should have at least two columns in which the first column contains the classes in the input map and the second column contains the new classes to be assigned
}
\value{
The output is a reclassified raster map
}
\references{
Robert Hijman. Raster Package in R. https://www.rdocumentation.org/packages/raster
}
\author{
Christian Thine Omuto
}
\seealso{
depthSuit, classCode, rainSuit
}
\examples{
library(sp)
library(raster)
LUT=data.frame(map=c(1,2,3,4,5,6),new=c(100,20,30,40,60,80))
newmap=as(suitabinput["depthcodes"],"RasterLayer")
newmap$depth=reclassifyMap(newmap$depthcodes,LUT)
summary(newmap$depth)
spplot(newmap$depth)
}