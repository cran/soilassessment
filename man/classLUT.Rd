\name{classLUT}
\alias{classLUT}
\title{
A function for developing Look-up Table (LUT) for soil condition class map
}
\description{
This function develops a Look-Up Table (LUT) for class type soil condition map. LUT is important when developing map legends or for re-classifying the maps.
}
\usage{
classLUT(fgrid,indicator)
}
\arguments{
  \item{fgrid}{
The input raster map containing class characters whose unique values are sought.
}
\item{indicator}{
The soil condition indicator of interest as contained in the input map for example, "texture", "salinity", etc.
}
}
\details{
The input raster map should contain only one band for the soil indicator or clear identification of the band with the indicator information like raster$texture, raster$fertility
}
\value{
The output is data frame containing classes in the map and corresponding unique number for a numeric map
}
\author{
Christian Thine Omuto
}

\seealso{
classCode, deriveclasses
}
\examples{
textrd=suitabinput["texture"]
\donttest{
LUT=classLUT(textrd,"texture")
LUT
}
}
