\name{regmodelSuit}
\alias{regmodelSuit}
\title{
A function for guiding regression model selection in soil condition modelling
}
\description{
This function evaluates seven regression models for their suitability in mapping soil condition using a set of predictors
}
\usage{
regmodelSuit(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
a set of target soil property and predictor vectors or columns of variables in a database
}
}
\details{
The target soil property and predictor vectors are seperated by commas. For example, in a soil database, the variable x = soil$EC, soil$landcover,soil$DEM, etc. The first vector set contains the target variable (e.g. EC in soil$EC) and the subsequent vectors contain the predictors. The vectors must be the columns of a complete database.
}
\value{
A table of model statistics such as root mean square error (RMSE), mean absolute error (MAE), r-squared (R2) and Nash-Sutcliffe coefficient of efficiency (NSE) for the seven popular models in digital soil mapping
}
\references{
Yigini, Y., Olmedo, G.F., Reiter, S., Baritz, R., Viatkin, K. and Vargas, R. (eds). 2018. Soil Organic Carbon Mapping Cookbook 2nd edition. Rome, FAO. 220 pp

Nash, J. E.; Sutcliffe, J. V. 1970. River flow forecasting through conceptual models part I — A discussion of principles. Journal of Hydrology. 10 (3): 282–290
}
\author{
Christian Thine Omuto
}
\note{
The function carries 5-fold cross-validation. Sometimes it may give warning of missing resample performance with cubist or CART models
}

\seealso{
suitability, pedoTransfer
}
\examples{
library(caret)
library(sp)
data(soil)
soil1=subset(soil,!is.na(soil$EC))
overlay.ov=over(soil1, suitabinput)
soil1$dem=overlay.ov$dem
soil1$rain=overlay.ov$rain
soil1$texture=overlay.ov$texture
\donttest{
regmodelSuit(soil1$EC,soil1$dem,soil1$rain,soil1$PH,soil1$Clay)
}
}
