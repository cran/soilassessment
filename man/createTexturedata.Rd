\name{createTexturedata}
\alias{createTexturedata}
\title{
A function for creating spatial dataframe of normalized soil texture proportions
}
\description{
The function creates spatial dataframe of normalized soil texture proportions. They are normalized so that they sum up to 100 per cent.
}
\usage{
createTexturedata(clay,silt,sand)
}

\arguments{
  \item{clay}{
clay proportion of soil texture in per cent
}
 \item{silt}{
silt proportion of soil texture in per cent
}
 \item{sand}{
sand proportion of soil texture in per cent
}
}
\details{
the input data of soil texture proportions are imported into R as spatial raster or dataframe. If in raster map, they need to have uniform coordinate reference system (CRS), same pixel size (resolution). The sum of the proportions should be close to 100 per cent for each row
}
\value{
The output is a spatial pixel dataframe of normalized soil texture proportions (for each pixel)
}
\author{
Christian Thine Omuto
}
\note{
It's important to ensure the input data does not have negative values nor add up to far below or above 100 per cent. It's also important to adhere to the order of the input data: clay, silt, sand
}

\seealso{
textureclassLUT, textureLevels,appendTextureclass
}
\examples{
#data(textureinput)
newmap=textureinput
\donttest{
texturedata=createTexturedata(newmap$clay, newmap$silt, newmap$sand)
cor(texturedata$CLAY,texturedata$CLAY_n)^2
}
}
