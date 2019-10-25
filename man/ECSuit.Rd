\name{ECSuit}
\alias{ECSuit}
\title{
A function for assessing salinity suitability requirements for certain crops
}
\description{
This function determines the suitability classes for salinity requirements of selected agricultural crops
}
\usage{
ECSuit(value, crop)
}
\arguments{
  \item{value}{
Input electrical conductivity in dS/m.
}
\item{crop}{
The crop of interest for which salinity suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of electrical conductivity (ECe) of saturated paste extract or its equivalent in dS/m
}
\value{
The output is EC suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
}
\references{
Sys, C., Van Ranst, E., Debaveye, J. and Beerneaert, F.1993. Land evaluation: Part III: Crop requirements.
Development Cooperation, Belgium.

Naidu, L.G.K., Ramamurthy, V., Challa O., Hegde, R. and Krishnan, P. 2006. Manual, Soil-site Suitability Criteria for Major Crops, National Bureau of Soil Survey and Land Use Planning, ICAR, Nagpur, India

FAO Crop Suitability Requirements: http://ecocrop.fao.org/ecocrop/srv/en/home
}
\author{
Christian Thine Omuto
}
\note{
If the input value is raster map, then the output will also be a raster map of salinity suitability for the crop of interest
}

\seealso{
tempSuit, PHSuit, rainSuit
}
\examples{
library(sp)
ECSuit(0.78,"yam")
ec=suitabinput["ec"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ECe=ECconversion1(ec$ec,soc$soc,clay$clay,texture$texture,"1:2.5", "FAO")
newmap$wheat=ECSuit(newmap$ECe,"wheat")
spplot(newmap["wheat"], main="EC suitability for wheat")
summary(newmap$wheat)
}