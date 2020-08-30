saltRating=function(ec,ph,criterion="FAO"){
  ph=ph*1
  if(criterion=="FAO"){
    b1=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
    saltClass=ifelse(ph<8.2,b1,ifelse(ec<4,4,5))
  }
  else if(criterion=="USDA"){
    b2=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
    saltClass=ifelse(ph<8.5,b2,ifelse(ec<4,4,5))
  }
  return(salinity=saltClass)
}
