\name{saltClass}
\alias{saltClass}
\title{
A function to classify types of salt-affected soils using EC, PH, and ESP
}
\description{
This function determines the major classes of salt-affected soils using Electrical Conductivity (EC), soil reaction (pH), and Exchangeable Sodium Percent (ESP) according to FAO or USDA classification schemes
}
\usage{
saltClass(ec,ph,ESP,criterion="FAO")
}
\arguments{
  \item{ec}{
Electrical Conductivity in dS/m of saturated soil paste extract or its equivalent
}
\item{ph}{
soil reaction (pH)
}
\item{ESP}{
Exchangeable Sodium Percent
}
  \item{criterion}{
The criterion to use for classifying the soil problem. Either FAO or USDA can be selected
}
}
\value{
saltClass returns integer classes of salt problems in the soil. The classes are 1, 2, 3, 4, 5 corresponding to None, Saline, Saline-sodic, Sodic, and Alkaline categories.
}
\references{
FAO.2006. Guidelines for soil description. FAO. Rome

Richards, L. A. (ed.) 1954. Diagnosis and Improvement of Saline and Alkali Soils. U.S. Department Agriculture Handbook 60. U.S. Gov. Printing Office, Washington, DC.

}
\author{
Christian Thine Omuto
}
\note{
ESP is mandatory when using this function. The "error: 1 * ESP : non-numeric argument to binary operator" is flagged when ESP entry is missing. In case ESP is missing, saltRating is suggested.
}

\seealso{
saltRating, saltSeverity, classnames, classCode
}
\examples{
library(sp)
saltClass(6.12,7.84, ESP=1,"FAO")

ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph
newmap$ECe=ECconversion1(ec$ec,soc$soc,clay$clay,texture$texture,"1:2.5", "FAO")
newmap$salinity=saltClass(newmap$ECe,newmap$ph, ESP=1,"FAO")
newmap$saltclass=classCode(newmap$salinity,"saltclass")
newmap$salineclass1=as.factor(newmap$saltclass)
spplot(newmap["saltclass"], main="Classes of salt-affected soils")
summary(newmap$salineclass1)
}
