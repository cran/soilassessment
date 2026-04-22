CECSuit=function(value,crop="wheat"){
  cropp1=c("groundnut","bean","pea","gram","soybean","lentil",
           "poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cashew","coconut","almond","pistachio","onion",
           "sweetpotato","cassava","potato","carrot","turnip","radish",
           "maize","wheat","rice","sorghum","millet","oat","barley",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado")
  cropp2=c("cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco")
  cropp3=c("cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","rosemary","parsley",
           "alfalfa","rose","jasmine","yam","pumpkin","butternut","squash")

  if(crop%in%cropp1){suitclass=calcSuit(value,8,16,24,400)}
  else if(crop%in%cropp2){suitclass=calcSuit(value,5,10,20,400)}
  else if(crop%in%cropp3){suitclass=calcSuit(value,5,10,15,400)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
