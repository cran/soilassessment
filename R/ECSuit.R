ECSuit=function(value, crop="wheat"){
  cropp1=c("cabbage","tomato","vegetable","broccoli","cauliflower","okra",
           "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley",
           "alfalfa","rose","jasmine",
           "yam","pumpkin","butternut","squash",
           "maize","millet","bean","soybean","lentil","gravillea",
           "calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","castor","mustard","mustard","rubber","turnip","radish", "banana","pawpaw")
  cropp2=c("wheat","sorghum","groundnut","poplar","oilpalm","sweetpotato","cassava",
           "potato","carrot","melon", "avocado","pomegranate")
  cropp3=c("barley","rice","cotton","cashew","coconut","almond","pistachio",
           "safflower","pyrethrum", "sugarcane")
  cropp4=c("pea","gram","mango","grape", "citrus","tobacco","tea","coffee")

  if(crop%in%cropp1){suitclass=getSuit(value,1,2,4,1000)}
  else if(crop%in%cropp2){suitclass=getSuit(value,2,4,6,1000)}
  else if(crop%in%cropp3){suitclass=getSuit(value,2,6,15,1000)}
  else if(crop%in%cropp4){suitclass=getSuit(value,0.5,1,2,1000)}
  else {stop("No crop chosen or crop not yet included in database")}
  return(suitclass)
}
