saltSeverity=function(ec,ph,ESP,method="FAO"){
  esp=1*ESP
  ec=ec*1
  #None=1;Saline=2;Saline_sodic=3;Sodic=4;Alkaline=5
  if(method=="FAO"){
    salt1=saltClass(ec,ph,ESP,"FAO")
    #salinity:1-6
    bb1=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
    #sodicity
    bb2=ifelse(esp<15,13,ifelse(esp<30,14,ifelse(esp<50,15,ifelse(esp<70,16,17))))
    saltclass=ifelse(salt1<3,bb1,ifelse(salt1<4,3,ifelse(salt1<5,bb2,18)))

  }
  else if(method=="USDA"){
    #salinity:1-6
    salt2=saltClass(ec,ph,ESP,"USDA")
    bb1=ifelse(ec<2,6,ifelse(ec<4,7,ifelse(ec<8,9,ifelse(ec<16,10,11))))
    #sodicity
    bb2=ifelse(esp<15,13,ifelse(esp<30,14,ifelse(esp<50,15,ifelse(esp<70,16,17))))
    saltclass=ifelse(salt2<3,bb1,ifelse(salt2<4,3,ifelse(salt2<5,bb2,14)))
  }
  else if(method=="Amrhein"){
    #salinity:1-6
    bb1=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
    #sodicity
    bb2=ifelse(esp<6,13,ifelse(esp<10,14,ifelse(esp<15,15,ifelse(esp<25,16,17))))
    saltclass=ifelse(salt1<3,bb1,ifelse(salt1<4,3,ifelse(salt1<5,bb2,14)))
  }
  return(saltclass)
}
