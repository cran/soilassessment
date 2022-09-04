\name{Mod}
\alias{Mod}
\docType{data}
\title{
Database of models in the Harmonization Service
}
\description{
A list of models included in the harmonization Service
}
\usage{data("Mod")}
\format{
  A data frame with 75 observations on the following 5 variables.
  \describe{
    \item{\code{number}}{a numeric index of list of available models in the Service}
    \item{\code{variable}}{a character vector of input data}
    \item{\code{extract}}{a character vector of soil extract used in data measurement}
    \item{\code{model}}{a character vector of name of model}
    \item{\code{texture}}{a character vector indicating need for texture information}
  }
}
\details{
This is a database of models ucrrently contained in the Harmonization Service
}
\examples{
data(Mod)
str(Mod)
}
\keyword{datasets}
