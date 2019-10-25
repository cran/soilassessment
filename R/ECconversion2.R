ECconversion2=function(ec,soilsolution="1:1", method="USDA"){

  if(method=="landon"){
    if(soilsolution=="1:1"){ElectConduct=ec*2.2}
    else if(soilsolution=="1:5"){ElectConduct=ec*6.4}
    else if(soilsolution=="1:3"){ElectConduct=ec*3.81}
  }
  else if(method=="kargas"){
    if(soilsolution=="1:1"){ElectConduct=ec*1.83+0.117}
    else if(soilsolution=="1:5"){ElectConduct=ec*6.53-0.108}
  }
  else if(method=="ozkan"){
    if(soilsolution=="1:1"){ElectConduct=ec*1.93-0.57}
    else if(soilsolution=="1:1.25"){ElectConduct=ec*3.3-0.2}
    else if(soilsolution=="1:5"){ElectConduct=ec*5.97-1.17}
  }
  else if(method=="USDA") {
    if(soilsolution=="1:1"){ElectConduct=ec*3}
    else if(soilsolution=="1:1.5"){ElectConduct=ec*4.5}
    else if(soilsolution=="1:2"){ElectConduct=ec*5}
  }
  else if(method=="hogg") {
    if(soilsolution=="1:1"){ElectConduct=ec*1.75-0.37}
    else if(soilsolution=="1:2"){ElectConduct=ec*1.38-0.14}
  }
  else if(method=="zhang") {
    if(soilsolution=="1:1"){ElectConduct=ec*1.79+1.46}
      }
  else if(method=="chi") {
    if(soilsolution=="1:5"){ElectConduct=ec*11.68-5.77}
  }

  return(ElectConduct)
}
