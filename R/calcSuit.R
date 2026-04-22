calcSuit=function(x,lm1,lm2,lm3,lm4){
 if(is.numeric(x)){
   classj=ifelse(x<0,NA,ifelse(x>lm4,NA,ifelse(x>lm3,1,ifelse(x>lm2,2,ifelse(x>lm1,3,4)))))
 }
  else {
    classj=ifel(x<0,NA,ifel(x>lm4,NA,ifel(x>lm3,1,ifel(x>lm2,2,ifel(x>lm1,3,4)))))
  }
  return(classj)
}
