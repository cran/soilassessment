\name{classCode}
\alias{classCode}
\title{
A function to displaying names of corresponding class codes in soilassessment package
}
\description{
This function displays character names of numerical classes (or levels) of derived codes for the soil condition
}
\usage{
classCode(value, indicator)
}
\arguments{
  \item{value}{
Numerical value of the soil condition indicator
}
 \item{indicator}{
Soil condition indicator whose class (x) is sought. The default = "fertility" if fertility is the soil condition
}
}
\details{
This is for interpretation of what the numerical value (x) means for the soil condition indicator.
}
\value{
Output character for the level of soil condition indicator
}

\seealso{
textureclassLUT, textureLevels, erodFUN
}
\examples{
classCode(2,"texture")
suitclas=classCode(4,"suitability")
levels(suitclas)
}
