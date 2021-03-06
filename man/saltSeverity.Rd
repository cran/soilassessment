\name{saltSeverity}
\alias{saltSeverity}
\title{
A function to classify severity levels of salt problems in the soil
}
\description{
This function classifies degree/severity of salt problems in the soil according to EC, pH and ESP levels
}
\usage{
saltSeverity(ec,ph,ESP,method="FAO")
}
\arguments{
  \item{ec}{
electrical conductivity in dS/m of saturated soil paste extract or its equivalent
}
 \item{ph}{
soil reaction (pH)
}
 \item{ESP}{
Exchangeable sodium percent
}
 \item{method}{
classification method for severity/degree of salt problems. FAO, USDA and Amrhein methods are included. Default method is FAO
}
}
\details{
This function requires input EC, pH and ESP values to process the classification. They can be maps or numerical entries
}
\value{
Integer classes of ranging between 1-17. The names of integer codes are obtained using classCode function
}
\references{
Abrol, IP, Yadav JSP, Massoud FI. 1988. Salt-affected soils and their management. FAO Soils Bulletin 39. FAO, Rome

Amrhein C. 1996. Australian sodic soils: Distribution, properties, and management. Soil Science 161. pp412.

FAO. 2006. Guidelines for soil description. FAO, Rome

Richards LA. 1954. Diagnosis and improvements of saline and alkali soils. Agriculture Handbook No. 60. USDA, Washington
}
\author{
Christian Thine Omuto
}
\note{
The function strictly requires input EC, pH, and ESP
}

\seealso{
saltClass, classCode, saltRating
}
\examples{
library(sp)
library(rgdal)
saltSeverity(4.5,7.8,11.6,"USDA")
ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph
newmap$ECe=ECconversion1(ec$ec*0.1,soc$soc,clay$clay,texture$texture,"1:5", "FAO")
newmap$salt=saltSeverity(newmap$ECe,newmap$ph,0.84,"FAO")
newmap$salineclass=classCode(newmap$salt,"saltseverity")
spplot(newmap["salineclass"], main="Salinity Code")
}
