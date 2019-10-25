salinityRating1=function(ec,ph,ESP,method="FAO"){
  ESP=1*ESP
  ph=ph*1
  bb0 = ifelse(ph<8.2,0,2)
  if(method=="FAO"){
    bb1=ifelse(ec<0.75,1,ifelse(ec<2,2,ifelse(ec<4,3,ifelse(ec<8,4,ifelse(ec<15,5,6)))))
    bb2=ifelse(ESP<15,bb1,9)
    saltClass=ifelse(bb0<1,bb2,ifelse(ESP>15,8,10))

  }
  else if(method=="USDA"){
    bb3=ifelse(ec<2,1,ifelse(ec<4,7,ifelse(ec<8,2,ifelse(ec<16,3,4))))
    bb4=ifelse(ESP<15,bb3,9)
    saltClass=ifelse(bb0<1,bb4,ifelse(ESP>15,8,10))
  }
  #Salinity classes: 1-None; 2-Slight; 7-Very Slight; 3-Moderate; 4-Strong; 5-Very Strong; 6 - Extreme; 8 - Sodic; 9-Saline_sodic; 10-Alkaline; 11-Severe; 12-Solonchak
  return(salinity=saltClass)
}
