saltRating=function(ec,ph,criterion="FAO"){
  ph=ph*1
  if(criterion=="FAO"){
    b1=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
    saltClass=ifelse(ph<8.5,b1,ifelse(ec<4,5,4))
  }
  else if(criterion=="USDA"){
    b2=ifelse(ec<2,6,ifelse(ec<4,7,ifelse(ec<8,9,ifelse(ec<16,10,11))))
    saltClass=ifelse(ph<8.5,b2,ifelse(ec<4,5,4))
  }
  #Salinity classes: 1-None; 2-Slight; 7-Very Slight; 3-Moderate; 4-Strong; 5-Very Strong; 6 - Extreme; 8 - Sodic; 9-Saline_sodic; 10-Alkaline; 11-Severe; 12-Solonchak
  return(salinity=saltClass)
}
