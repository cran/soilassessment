ESPSuit=function(value, crop="wheat"){
  cropp1=c("cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash",
           "maize","sorghum","barley","oat",
           "groundnut","pea","gram","soybean","lentil",
           "gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "cassava","potato","carrot","turnip","radish",
           "sesame","sunflower","safflower","mustard","rapeseed","olive",
           "mango","grape","citrus","pomegranate","banana","avocado",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco")
  cropp2=c("cotton","poplar","bean","pineaple","watermelon","melon","pawpaw","castor","oilpalm")
  cropp3=c("cashew","alfalfa","rice")
  cropp4=c("sweetpotato","wheat")

  if(crop%in%cropp1){suitclass=getSuit(value,2,10,15,1000)}
  else if(crop%in%cropp2){suitclass=getSuit(value,5,10,20,1000)}
  else if(crop%in%cropp3){suitclass=getSuit(value,10,20,50,1000)}
  else if(crop%in%cropp4){suitclass=getSuit(value,3,5,10,1000)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
