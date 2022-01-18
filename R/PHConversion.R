PHConversion=function(ph, model="kabala",phtype="CaCl2"){
  if(phtype=="KCl"){
    if(model=="kabala"){PHconvert=(-1.95)+11.58*log10(ph)}
    else if(model=="sadovski"){PHconvert=1.53466+0.88787*ph}
  else if(phtype=="CaCl2"){
    if(model=="miller"){PHconvert=0.9259259*((0.973+ph))}
    else if(model=="davies"){PHconvert=0.9569378*(0.876+ph)}
    else if(model=="brennan"){PHconvert=1.089325*(0.3556+ph)}
  }
  }
  return(PHconvert)
}
