\name{SASdata_densityInfo}
\alias{SASdata_densityInfo}
\title{
Global spatial distribution of locations with measured soil properties for salt-affected soils (SAS) information
}
\description{
Global distribution of sampling points with measured soil property data for SAS information
}
\usage{
SASdata_densityInfo(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
type of measured soil data in the global database of SAS information. There are three categories of soil data: ec, ph, texture.
}
}
\details{
The function accepts three input alternatives for querrying available SAs information: "ec", "ph", and "texture". The default is "ec"
}
\value{
Spatial maps of sampling locations with measured soil data for SAS information. They include maps of electrical conductivity (ec), pH, texture (sand, silt, clay percentages), and cation exchange capacity (CEC). Locations for CEC are similar to those for texture.
}
\references{
Batjes, N.H., Ribeiro, E., van Oostrum, A., 2020. Standardised soil profile data to support global mapping and modelling (WoSIS snapshot 2019). Earth Syst. Sci. Data 12, 299–320. https://doi.org/10.5194/essd-12-299-2020

FAO/IIASA/ISRIC/ISS-CAS/JRC, 2012. Harmonized World Soil Database (version 1.2). FAO and IIASA, Rome.

}
\author{
Christian Thine Omuto
}
\note{
The function currently works for ec, ph, and texture. Distribution of locations for texture is similar to those for CEC. The input for this function must be entered in quotation marks.Due to periodic update,internet connectivity is needed for the function to work.
}

\seealso{
ECharm_Info, PHharmInfo
}
\examples{

SASdata_densityInfo("ec")
}

