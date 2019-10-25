pedoTrasnfer=function (clay, sand, silt, oc, ph)
{
  .value <- clay + sand + silt + oc + ph
  .grad <- array(0, c(length(.value), 1L), list(NULL, c("A")))
  .grad[, "A"] <- 0
  attr(.value, "gradient") <- .grad
  .value
}
