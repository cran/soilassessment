\name{permeabilityClass}
\alias{permeabilityClass}

\title{
A function to determine permeability class
}
\description{
This function determines the soil permeability classes according to the USDA soil textural classes
}
\usage{
permeabilityClass(texture)
}
\arguments{
  \item{texture}{
soil textural class code
}
}
\details{
Soil textural class code can be obtained using classnames("texture")
}
\value{
permeability class code
}
\references{
O'Geen, A. T. (2013) Soil Water Dynamics. Nature Education Knowledge 4(5):9

Soil Survey Staff. Soil Taxonomy A Basic System of Soil Classification for Making and Interpreting Soil Surveys. Agricultural Handbook No. 436. U.S. Government Printing Office Washington, DC, 1999.
}
\author{
Christian Thine Omuto
}

\seealso{
drainageClass, erodFUN, erosivFUN
}
\examples{
library(sp)
permeabilityClass(11)

texture=suitabinput["texture"]
texture$permeability=permeabilityClass(texture$texture)
str(texture$permeability)
spplot(texture["permeability"])
}
