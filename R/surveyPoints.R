surveyPoints=function(soilmap,scorpan,conditionclass,mapproportion){
  if(is(soilmap,"RasterLayer")){
  map=soilmap;
  w=scorpan;
  b=conditionclass;
  j=mapproportion
  tagt<-map==b
  area=length(subset(getValues(tagt), getValues(tagt) == 1)) * res(tagt)^2
  w=ifelse(w<1,1,ifelse(w>5,5,w))
  area=area[1]
  samplnum=(j/100)*area*(1/(4*w*res(tagt)))^2
  samplnum=round(samplnum[1],0)
  ndat=sampleRandom(tagt,samplnum, xy=TRUE, sp=TRUE)
  ndat=subset(ndat,ndat$layer>0)}
  else {
    w=scorpan;
    b=conditionclass;
    j=ifelse(mapproportion<0,0,ifelse(mapproportion>100,100,mapproportion))
    tagt<-map==b
    area=length(subset(getValues(tagt), getValues(tagt) == 1)) * res(tagt)^2
    w=ifelse(w<1,1,ifelse(w>5,5,w))
    area=area[1]
    samplnum=(j/100)*area*(1/(4*w*res(tagt)))^2
    samplnum=round(samplnum[1],0)
    ndat=sampleRandom(tagt,samplnum, xy=TRUE, sp=TRUE)
    ndat=subset(ndat,ndat$layer>0)}
    return(ndat)
}
