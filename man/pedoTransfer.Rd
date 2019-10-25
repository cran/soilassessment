\name{pedoTrasnfer}
\alias{pedoTrasnfer}
\title{
A pedotransfer function to predict electrical conductivity using texture, organic carbon and pH
}
\description{
This generic pedo-transfer function which can be used to get approximate EC values of existing soil data that do not have the EC measurements
}
\usage{
pedoTrasnfer(clay, sand, silt, oc, ph)
}
\arguments{
  \item{clay}{
clay content in percent
}
  \item{sand}{
sand content in percent
}
  \item{silt}{
silt content in percent
}
  \item{oc}{
organic carbon content in percent
}
  \item{ph}{
soil reaction (pH)
}
}
\details{
This generic model can be used even with other soil properties expected to influence EC. For example, instead of clay, one can put calcium carbonate; instead of sand, one can put exchangeable acidity, etc. The model needs training data to establish the exact predictive model for other applications
}
\value{
model for predicting EC given similar input data
}
\references{
van Looy k,  Bouma  J, Herbst M,  Koestel J, Minasny B,  Mishra U, Montzka C,  Nemes A,  Pachepsky AY,  Padarian J,  Schaap MG,  Tóth B,  Verhoef A, Jan Vanderborght,  van der Ploeg MJ,  Weihermüller L,   Zacharias S,  Zhang Y,  Vereecken H. 2017. Pedotransfer functions in Earth System Science: Challenges and Perspectives. Reviews of Geophysics 55(4): 1199-1256.

Sudduth KA, Kitchen RN, Wiebold WJ, Batchelor W. 2005. Relating apparent electrical conductivity to soil properties across the North-Central USA. Computers and Electronics in Agriculture, 46(1-3):263-283
}
\author{
Christian Thine Omuto
}
\note{
This function can also be used to predict EC from apparent electrical conductivity of bulk soil, texture, and other important soil properties
}

\seealso{
ECconversion1, ECconversion2, SalinityRating1, SalinityRating2
}
\examples{
clay=as.data.frame(runif(20, 1,100))
silt=as.data.frame (runif(20,20,70))
sand=as.data.frame(runif(20,10.1,50.5))
pH=as.data.frame(runif(20,1,14))
EC=as.data.frame(runif(20,0.5,20.5))
OC=as.data.frame(runif(20,0.1,1.25))
newsoil=cbind(EC,clay,silt,sand,OC,pH)
names(newsoil)=c("EC","clay","silt","sand","OC","pH")
EC1.lm=lm(EC~pedoTrasnfer(clay,sand,silt,OC,pH),data=newsoil)
cor(newsoil$EC,fitted(EC1.lm))^2
plot(newsoil$EC~fitted(EC1.lm))
abline(1,1)
}
