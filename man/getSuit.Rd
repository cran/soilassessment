\name{getSuit}
\alias{getSuit}
\title{
Deriving categorical suitability classes
}
\description{
A generic model to derive suitability classes given the target information.
The function uses provided limits to derive the suitability classes
}
\usage{
getSuit(x,lim1,lim2,lim3,lim4)
}
\arguments{
  \item{x}{
  Input value of the indicator to be classified
}
  \item{lim1}{
  Lower limit of the highest class
}
  \item{lim2}{
  Lower limit of the second highest class
}
  \item{lim3}{
  Lower limit of the second last class
}
  \item{lim4}{
  Maximum allowable limit for target variable
  }
}
\details{
The function puts the boundary conditions for realistic values of target soil properties
}
\value{
Suitability class values as integers. 1-High, 2-Moderate, 3-Low, 4-Not suitable
}
\author{
Christian Thine Omuto
}
\note{
Negative values are not permitted and will return NA. Similarly, large values out of range are also not permitted}

\seealso{
\code{\link{classLUT}}, \code{\link{classCode}}
}
\examples{
getSuit(1.67,1.2,1.3,1.5,2.6)
}
