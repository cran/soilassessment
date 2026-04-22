\name{calcSuit}
\alias{calcSuit}
\title{
Calculating suitability classes
}
\description{
Function for calculating suitability classes given the target information.
The function use given limits to calculate the suitability class
}
\usage{
calcSuit(x, lm1,lm2,lm3,lm4)
}
\arguments{
\item{x}{
  Input value of the indicator to be classified
}
  \item{lm1}{
  Upper limit of the highest class
}
  \item{lm2}{
  Upper limit of the second highest class
}
  \item{lm3}{
  Upper limit of the second last class
}
  \item{lm4}{
  Maximum allowable limit for target variable
  }
}
\details{
The function calculates the boundary conditions for realistic values of target soil properties
}
\value{
An interger representing suitability class. 1-High, 2-Moderate, 3-Low, 4-Not suitable
}
\author{
Christian Thine Omuto}
\note{
Negative values are not permitted and will return NA. Similarly, large values out of range are also not permitted
}

\seealso{
\code{\link{classCode}}
}
\examples{
calcSuit(22,8,12,24,100)
}
