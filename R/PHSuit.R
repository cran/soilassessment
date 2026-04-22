PHSuit=function(value, crop="wheat"){
  cropp1=c("wheat","groundnut","gram","lentil","poplar","calliandra","acacia","eucalyptus","teak","maple",
           "ash","sugarcane")
  cropp2=c("maize","sorghum","millet","oat","bean","pea","soybean","grevillea","sesbania","leucaena",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cashew","cotton","jute","tobacco","sweetpotato","cassava","potato","carrot","turnip","radish",
           "mango","pomegranate","banana","cabbage","tomato","vegetable","okra", "alfalfa","rose","jasmine",
           "chilli","pepper","tumeric")
  cropp3=c("rice","barley","coconut","almond","pistachio","coffee","saffron","grape","citrus","watermelon",
           "melon","pawpaw","broccoli","cauliflower","onion","ginger","lemongrass","cardamom")
  cropp4=c("tea","pyrethrum","rubber","pineaple","avocado","yam","pumpkin","butternut","squash","vanilla"
           )
  if(crop%in%cropp1){suitclass=getSuit(value,6.5,7.5,8,14)}
  else if(crop%in%cropp2){suitclass=getSuit(value,5.6,7,8.1,14)}
  else if(crop%in%cropp3){suitclass=getSuit(value,5.2,6,7.8,14)}
  else if(crop%in%cropp4){suitclass=getSuit(value,5,6.1,7.5,14)}
  else {stop("No crop chosen or crop not yet included in database")}

   return(suitclass)
}
