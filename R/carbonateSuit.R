carbonateSuit=function(value, crop="wheat"){
  cropp1 = c("pea","soybean","lentil","sesbania","acacia","leucaena","eucalyptus","maple","cabbage","grevillea","ash",
           "sesame","bean","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive","alfalfa",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco","teak")
  cropp2 = c("rice","gram","calliandra","grape")
  cropp3 = c("chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion",
           "rosemary","parsley","rose","jasmine","yam","pumpkin","butternut","squash",
           "vegetable","broccoli","cauliflower","okra")
  cropp4 = c("sorghum","millet","oat","barley")
  cropp5 = c("cassava","mango","citrus","tomato")
  cropp6 = c("groundnut","poplar","cashew","coconut","almond","pistachio", "maize","wheat")
  cropp7 = c("pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
             "sweetpotato","potato","carrot","turnip","radish")

   if    (crop%in%cropp1){suitclass=getSuit(value,6,10,25,100)}
  else if(crop%in%cropp2){suitclass=getSuit(value,10,20,30,100)}
  else if(crop%in%cropp3){suitclass=getSuit(value,4,10,15,100)}
  else if(crop%in%cropp4){suitclass=getSuit(value,3,10,25,100)}
  else if(crop%in%cropp5){suitclass=getSuit(value,0.5,5,10,100)}
  else if(crop%in%cropp6){suitclass=getSuit(value,12,25,35,100)}
  else if(crop%in%cropp7){suitclass=getSuit(value,5,15,30,100)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
