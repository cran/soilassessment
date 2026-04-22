demSuit=function(value, crop="wheat"){
  cropp1=c("maize","wheat","rice","sorghum","millet","oat","barley",
           "groundnut","bean","pea","gram","soybean","lentil",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash")
  cropp2=c("poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "cashew","coconut","almond","pistachio")
  cropp3=c("sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive")
  cropp4=c("sweetpotato","cassava","potato","carrot","turnip","radish",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley"
           )

  if(crop%in%cropp1){suitclass=getSuit(value,1750,1830,1900,10000)}
  else if(crop%in%cropp2){suitclass=calcSuit(value,1750,1830,2050,10000)}
  else if(crop%in%cropp3){suitclass=calcSuit(value,1750,1830,2000,10000)}
  else if(crop%in%cropp4){suitclass=calcSuit(value,1750,1830,1850,10000)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}

