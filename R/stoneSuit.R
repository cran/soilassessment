stoneSuit=function(value, crop="wheat"){
  cropp1=c("maize","rice","sorghum","millet","oat",
           "groundnut","bean","pea","gram","soybean","lentil",
           "poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","safflower","olive",
           "sweetpotato","cassava","potato","carrot","turnip","radish",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash")
  cropp2=c("wheat","cashew","coconut","almond","pistachio","rapeseed","mustard","cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco")
  cropp3=c("barley","sweetpotato")

  if(crop%in%cropp1){suitclass=getSuit(value,5,15,35,100)}
  else if(crop%in%cropp2){suitclass=getSuit(value,15,35,50,100)}
  else if(crop%in%cropp3){suitclass=getSuit(value,0.2,0.5,0.75,100)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}

