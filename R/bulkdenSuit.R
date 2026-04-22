bulkdenSuit=function(value, crop="wheat"){
  cropp1=c("maize","wheat","rice","sorghum","millet","oat","barley",
           "groundnut","bean","pea","gram","soybean","lentil",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash","cassava")
  cropp2=c( "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
            "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley")
  cropp3=c("sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco")
  cropp4=c("poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "cashew","coconut","almond","pistachio")
  if(crop%in%cropp1){suitclass=getSuit(value,1.2,1.3,1.5,2.6)}
  else if(crop%in%cropp2){suitclass=getSuit(value,1.2,1.3,1.45,2.6)}
  else if(crop%in%cropp3){suitclass=getSuit(value,1.2,1.3,1.55,2.6)}
  else if(crop%in%cropp4){suitclass=getSuit(value,1.2,1.3,1.6,2.6)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
