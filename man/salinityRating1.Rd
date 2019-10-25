\name{salinityRating1}
\alias{salinityRating1}
\title{
A function for soil salinity classification using EC, PH, and ESP
}
\description{
This function determines the soil salinity class using Electrical Conductivity (EC), soil reaction (pH), and Exchangeable Sodium Percent (ESP) according to FAO (2006) or USDA classification scheme
}
\usage{
salinityRating1(ec, ph, ESP, method = "FAO")
}
\arguments{
  \item{ec}{
Electrical Conductivity in dS/m of saturated soil extract or its equivalent
}
  \item{ph}{
soil reaction (pH)
}
  \item{ESP}{
Exchangeable Sodium Percent
}
  \item{method}{
The method to use for classifying the soil salinity. Either FAO or USDA can be selected
}
}
\value{
The output is a salinity class in integer values. The class name for any integer code returned can be obtained from classCode function
}
\references{
FAO.2006. Guidelines for soil description. FAO. Rome

Richards, L. A. (ed.) 1954. Diagnosis and Improvement of Saline and Alkali Soils. U.S. Department Agriculture Handbook 60. U.S. Gov. Printing Office, Washington, DC.
}
\author{
Christian Thine Omuto
}
\note{
ESP is mandatory in order to use this function. The "error: 1 * ESP : non-numeric argument to binary operator" is flagged when ESP entry is missing. In case ESP is missing, salinityRating2 is suggested.
}

\seealso{
salinityRating2, salinityRating3, classnames, classCode
}
\examples{
library(sp)
salinityRating1(6.12,7.84, ESP=1,"FAO")

ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph
newmap$ECe=ECconversion1(ec$ec,soc$soc,clay$clay,texture$texture,"1:2.5", "FAO")
newmap$salinity=salinityRating1(newmap$ECe,newmap$ph, ESP=1,"FAO")
newmap$salineclass=classCode(newmap$salinity,"salinity")
newmap$salineclass1=as.factor(newmap$salineclass)
spplot(newmap["salinity"], main="Salinity Code")
summary(newmap$salineclass1)
}
