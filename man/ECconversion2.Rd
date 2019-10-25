\name{ECconversion2}
\alias{ECconversion2}
\title{
A function for correcting electrical conductivity of soil solution to that of saturated paste extract for all textural classes
}
\description{
This function converts electrical conductivity measurements of soil solution to that of soil paste extract. The ratio of soil:water mix for the solution and conversion method must be indicated
}
\usage{
ECconversion2(ec, soilsolution, method)
}
\arguments{
  \item{ec}{
measured electrical conductivity of the soil solution in dS/m
}
 \item{soilsolution}{
ratio of soil:water mix when electrical conductivity was measured. Example is 1:1, 1:2, etc. The default is 1:1
}
 \item{method}{
method for converting electrical conductivity of the soil:water mix to that of the soil paste extract. The methods included are USDA, landon, kargas, ozkan, and hogg. The default is USDA
}
}
\details{
This function assumes no influence of texture, clay content, etc on the conversion of electrical conductivities.
}
\value{
electrical conductivity equivalent for saturated soil extract in dS/m
}
\references{
Sonmez S, Buyuktas D, Asri FO. 2008. Assessment of different soil to water ratios (1:1, 1:2.5, 1:5) in soil salinity studies. Geoderma, 144: 361-369

Kargas G, Chatzigiakoumis I, Kollias A, Spiliotis D, Massas I, Kerkides P. 2018. Soil salinity assessment using saturated paste and mass soil:water 1:1 and 1:5 ratios extracts. Water, 10:1589,  doi:10.3390/w10111589
}
\author{
Christian Thine Omuto
}
\seealso{
ECconversion1, salinityRating1, salinityRating2
}
\examples{
library(sp)
ECconversion2(0.75,"1:1", "USDA")
newmap = suitabinput["ec"]
newmap$salinity=ECconversion2(newmap$ec,"1:1","hogg")
str(newmap$salinity)
spplot(newmap["salinity"])
}
