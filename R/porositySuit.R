porositySuit=function(value, crop="wheat"){
  cropp1 =c("maize","wheat","rice","sorghum","millet","oat","barley",
           "groundnut","bean","pea","gram","soybean","lentil",
           "poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cashew","coconut","almond","pistachio",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco",
           "sweetpotato","cassava","potato","carrot","turnip","radish")
  cropp2=c("mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash")

  if(crop%in%cropp1){suitclass=calcSuit(value,30,40,48,100)}
  else if(crop%in%cropp2){suitclass=calcSuit(value,33,40,48,100)}

  return(suitclass)
}
