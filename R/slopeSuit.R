slopeSuit=function (x,crop="wheat"){
  if(!missing(x)){warning("slope must be in degrees")}
  cropp1=c("wheat","groundnut","pea","gram","poplar","maple","sunflower","castor","almond","pistachio",
           "mango","grape","citrus","pomegranate","ash",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine","sesame","rapeseed",
           "yam","pumpkin","butternut","squash")
  cropp2=c("banana","potato","olive","mustard","gravillea","sorghum","bean")
  cropp3=c("barley","oat","lentil","calliandra","leucaena","acacia","eucalyptus","teak","safflower","cassava","potato","carrot","turnip","radish",
           "pineaple","pawpaw","avocado","oilplam")
  cropp4=c("millet","soybean","sesbania","rubber","coffee","tea","saffron","pyrethrum",
           "sweetpotato","melon","cashew","coconut")
  value=tan(x*pi/180)*100
  if(crop%in%cropp1){suitclass=getSuit(value,0.118,0.196,0.393,90)}
  else if(crop%in%cropp2){suitclass=getSuit(value,0.118,0.314,0.589,90)}
  else if(crop%in%cropp3){suitclass=getSuit(value,0.157,0.314,0.628,90)}
  else if(crop%in%cropp4){suitclass=getSuit(value,0.314,0.628,2,90)}
  else if(crop=="rice"){suitclass=getSuit(value,0.039,0.118,0.196,90)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
