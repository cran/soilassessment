erosionSuit=function(value,crop="wheat"){
  cropp =c("maize","wheat","rice","sorghum","millet","oat","barley",
           "groundnut","bean","pea","gram","soybean","lentil",
           "poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cashew","coconut","almond","pistachio",
           "cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco",
           "sweetpotato","cassava","potato","carrot","turnip","radish",
           "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
           "cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash")

  if(crop%in%cropp){suitclass=getSuit(value,2,5,7.5,1000)}

  return(suitclass)
}
