SOCSuit=function(value,crop="wheat"){
  cropp1=c("groundnut","bean","pea","gram","soybean","lentil","cotton","sugarcane","tea","coffee","rubber","jute",
           "poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","maize","grape",
           "potato","carrot","turnip","radish")
  cropp2=c("chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
            "alfalfa","rose","jasmine","rice","cashew","coconut","almond","pistachio",
            "yam","pumpkin","butternut","squash","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
            "cabbage","vegetable","broccoli","cauliflower","okra","cassava","rose","jasmine"
            )
  cropp3=c("wheat","sorghum","millet","oat")
  cropp4=c("vanilla","ginger","alfalfa","ginger","tumeric","vanilla","barley","safflower","mustard","rapeseed","olive",
           "saffron","pyrethrum","tobacco","sweetpotato","mango","tomato")

  if(crop%in%cropp1){suitclass=calcSuit(value,0.3,0.8,1.2,100)}
  else if(crop%in%cropp2){suitclass=calcSuit(value,0.3,0.8,1.5,100)}
  else if(crop%in%cropp3){suitclass=calcSuit(value,0.2,0.5,0.75,100)}
  else if(crop%in%cropp4){suitclass=calcSuit(value,0.5,0.98,2,100)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
