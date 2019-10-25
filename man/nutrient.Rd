\name{nutrient}
\alias{nutrient}
\docType{data}
\title{
Sample data of decision ranking table for mapping soil nutrient condition
}
\description{
This is an 11-factor table of decision ranking of soil nutrient indicators
}
\usage{data("nutrient")}
\format{
  A data frame with 11 factors for pairwise decision ranking between soil nutrient indicators.
   }
\details{
The ranks range between 1 and 9 and their reciprocals
}
\source{
Hypothetical data of rank between soil nutrient indicators
}
\examples{
data(nutrient)
str(nutrient)
plot(nutrient)
}
\keyword{datasets}
