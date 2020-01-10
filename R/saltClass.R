saltClass=function(ec,ph,ESP,criterion="FAO"){
  ec=ec*1
  ph=ph*1
  ESP=ESP*1
  if(criterion=="FAO"){
   bb1=ifelse(ESP>15,3,2)
   bb2=ifelse(ph>8.2,5,1)
   saltClass=ifelse(ec>4,bb1,ifelse(ESP>15,4,bb2))
      }
  else if(criterion=="USDA"){
    bb1=ifelse(ESP>15,3,2)
    bb2=ifelse(ph>8.5,5,1)
    saltClass=ifelse(ec>4,bb1,ifelse(ESP>15,4,bb2))
  }
  #Salinity classes: 1-None; 2-Saline; 3-Saline_sodic; 4-Sodic; 5-Alkaline;
  return(salinity=saltClass)
}
