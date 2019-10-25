salinityRating2=function(ec,ph,criterion="FAO"){
  ph=ph*1
  if(criterion=="FAO"){
    b1=ifelse(ec<0.75,1,ifelse(ec<2,2,ifelse(ec<4,3,ifelse(ec<8,4,ifelse(ec<15,5,6)))))
    saltClass=ifelse(ph<8.5,b1,ifelse(ec<4,8,9))
  }
  else if(criterion=="USDA"){
    b2=ifelse(ec<2,1,ifelse(ec<4,7,ifelse(ec<8,2,ifelse(ec<16,3,4))))
    saltClass=ifelse(ph<8.5,b2,ifelse(ec<4,8,9))
  }
  #Salinity classes: 1-None; 2-Slight; 7-Very Slight; 3-Moderate; 4-Strong; 5-Very Strong; 6 - Extreme; 8 - Sodic; 9-Saline_sodic; 10-Alkaline; 11-Severe; 12-Solonchak
  return(salinity=saltClass)
}
