fertilitySuit=function(value,crop="wheat"){
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
  if(crop%in%cropp1){suitclass=getSuit(value,1.2,2.4,3.3,10)}
  else if(crop%in%cropp2){suitclass=getSuit(value,1.2,2.4,3.3,10)}
  return(suitclass)
}
