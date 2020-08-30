ECconversion4=function(x,target="TDS"){

  if(target=="SAR"){
    bb=(-0.0126+0.01475*x)*100/(1+(-0.0126+0.01475*x))
    outSalt=ifelse(bb<0,0,bb)
  }
  else if(target=="TDS"){
    outSalt=ifelse(x<3200,x/640,x/800)
  }
  else if(target=="TSS"){
    outSalt=x/10
  }
  else if(target=="mmho"){
    outSalt=1*x
  }
   return(outSalt)
}

