\name{sloplenFUN}
\alias{sloplenFUN}
\title{
A function for estimating slope-length factor for soil erosion
}
\description{
Function for estimating the slope length factor used in erosion risk assessment
}
\usage{
sloplenFUN(ls,slope,method)
}
\arguments{
  \item{ls}{
length of slope in metres
}
 \item{slope}{
slope of land in degrees
}
 \item{method}{
method for deriving slope-length factor. The methods included are: WSmith, Renard, Remortel, Zhang, Nearing, Smith, Foster, David, Morgan, and Moore.
}
}
\details{
Slope (degrees) and length of slope (metres) are erosion inputs that are combined in this function to produce slope-length factor of soil erosion.
}
\value{
a dimensionless quantity of slope-length factor of erosion risk
}
\references{
Benavidez R, Bethana J, Maxwell D, Norton K. 2018. A review of the (Revised) Universal Soil Loss Equation ((R)USLE): with a view to increasing its global applicability and improving soil loss estimates. Hydrol. Earth Syst. Sci., 22, 6059–6086

Omuto CT and Vargas R. 2009. Combining pedometrics, remote sensing and field observations for assessing soil loss in challenging drylands: a case study of northwestern Somalia. Land Degrad. Develop. 20: 101–115
}
\author{
Christian Thine Omuto
}
\note{
The slope must be in degrees. The warning given is a reminder to that the slope is given in degrees
}

\seealso{
erosivFUN, erodFUN, slopeSuit
}
\examples{
library(sp)
library(raster)
sloplenFUN(60,14.88,"Renard")
dem=raster(suitabinput["dem"])
slope=terrain(dem,opt='slope')*180/pi
slope=as(slope,"SpatialGridDataFrame")
slope$LSrenard=sloplenFUN(60,slope$slope,"Renard")
summary(slope$LSrenard)
spplot(slope["LSrenard"])
}