\name{erosionSuit}
\alias{erosionSuit}
\title{
Assessing erosion risk for certain crops and trees
}
\description{
This function determines risk classes for erosion tolerance for selected agricultural crops and forest trees
}
\usage{
erosionSuit(value, crop)
}
\arguments{
  \item{value}{
Input erosion risk in ton/ha.
}
 \item{crop}{
The crop of interest for which erosion risk class is sought.
}
}
\details{
The input value can be map or just a numerical entry of erosion risk in g/cm3
}
\value{
The output is erosion risk class for the crop. The output is integer value for suitability class: 1- high; 2 - moderate; 3 - low; 4 - very low
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
Should the input value be raster map, then the output will also be a raster map of erosion risk class for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{PHSuit}}, \code{\link{fertilitySuit}}
}
\examples{
erosionSuit(1.78,"cotton")
}
