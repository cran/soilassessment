reclassifyMap=function(fgrid,df){
    if(class(fgrid)[1]=="SpatialGridDataFrame"){
    name1=names(fgrid@data[1])
    ccode=as(fgrid[name1],"RasterLayer")
    bcode=reclassify(ccode,df)
    bclass=as(bcode,"SpatialGridDataFrame")
    classcode=bclass@data[[1]]
  }
  else{
    classcode=reclassify(fgrid,df)
  }
  return(classcode)
}
