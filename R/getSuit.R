getSuit=function(x, lim1,lim2,lim3,lim4){
  if(is.numeric(x)){
    classj=ifelse(x<0,NA,ifelse(x>lim4,NA,ifelse(x>lim3,4,ifelse(x>lim2,3,ifelse(x>lim1,2,1)))))
  }
  else {
    classj=ifel(x<0,NA,ifel(x>lim4,NA,ifel(x>lim3,4,ifel(x>lim2,3,ifel(x>lim1,2,1)))))
    }
  return(classj)
}
