\name{imageIndices}
\alias{imageIndices}
\title{
A function for deriving remote sensing indices for soil assessment
}
\description{
The function determines commonly used remote sensing indices with some relationship with soil surface or vegetation cover.
}
\usage{
imageIndices(blue, green,red,nir,swir1,swir2,index)
}
\arguments{
  \item{blue}{
blue image band with wavelength range: 0.452-0.512 µm
}
\item{green}{
green image band with wavelength range: 0.533-0.59 µm
}
\item{red}{
red image band with wavelength range: 0.636-0.673 µm
}
\item{nir}{
NIR image band with wavelength range: 0.851-0.879 µm
}
\item{swir1}{
SWIR image band with wavelength range: 1.566-1.651 µm
}
\item{swir2}{
SWIR image band with wavelength range: 2.107-2.294 µm
}
\item{index}{
index from combination of image bands such as NDVI, SAVI, SI, etc. The default is NDVI.
}
}
\details{
The indices included in the package are: NDVI, NDSI (for salinity), NDSnI (for snow), SI1, SI2, SI3, SI4, SI5, si6, savi, vssi,ndsi, ndvi, SR, CRSI, BI, NDSnI, ROCK
}
\value{
dimensionless remote sensing index
}
\references{
Gorji T, Yildirim A, Sertel E, Tanik A. 2019. Remote sensing approaches and mapping methods for monitoring soil salinity under different climate regimes. International Journal of Environment and Geoinformatics 6(1): 33-49 (2019)
}
\author{
Christian Thine Omuto
}
\seealso{
featureRep
}
\examples{

imageIndices(0.15,0.05,0.18,0.25,0.36,0.45,"SAVI")

}
