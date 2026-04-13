\name{bulkdenSuit}
\alias{bulkdenSuit}
\title{
Assessing bulk density suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for bulk density requirements for selected agricultural crops and forest trees
}
\usage{
bulkdenSuit(value, crop)
}
\arguments{
  \item{value}{
Input bulk density in g/cubic cm (g/cm3).
}
 \item{crop}{
The crop of interest for which bulk density suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of bulk density mineral earth in g/cm3
}
\value{
The output is bulk density class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
Should the input value be raster map, then the output will also be a raster map of bulk density suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{PHSuit}}, \code{\link{fertilitySuit}}
}
\examples{
bulkdenSuit(1.78,"yam")
}
