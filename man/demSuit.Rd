\name{demSuit}
\alias{demSuit}
\title{
Assessing elevation suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for elevation requirements for selected agricultural crops and forest trees
}
\usage{
demSuit(value, crop)
}
\arguments{
  \item{value}{
Input elevation in meter.
}
 \item{crop}{
The crop of interest for which elevation suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of elevation in meters
}
\value{
The output is elevation suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
Should the input value be raster map, then the output will also be a raster map of elevation suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{PHSuit}}, \code{\link{fertilitySuit}}
}
\examples{
demSuit(1208,"maize")
}
