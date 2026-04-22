depthSuit=function(value, crop="wheat"){
  cropp1=c("maize","wheat","rice","sorghum","millet","oat","barley",
           "groundnut","bean","pea","gram","soybean","lentil",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash")
  cropp2=c("poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash")
  cropp3=c("mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "cashew","coconut","almond","pistachio",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco")
  cropp4=c("sweetpotato","cassava","potato","carrot","turnip","radish"
           )
  if(crop%in%cropp1){suitclass=calcSuit(value,25,50,75,1000)}
  else if(crop%in%cropp2){suitclass=calcSuit(value,105,150,175,1000)}
  else if(crop%in%cropp3){suitclass=calcSuit(value,50,75,200,1000)}
  else if(crop%in%cropp4){suitclass=calcSuit(value,25,50,75,1000)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
