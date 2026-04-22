physicalSuit=function(value, crop="wheat"){
cropp1=c("maize","wheat","rice","sorghum","millet","oat","barley",
          "groundnut","bean","pea","gram","soybean","lentil",
          "alfalfa","rose","jasmine",
         "mango","grape","citrus","pomegranate","banana","watermelon","melon","pineaple","pawpaw","avocado",
         "yam","pumpkin","butternut","squash")
cropp2=c("poplar","gravillea","sesbania","calliandra","leucaena","acacia","eucalyptus","teak","maple","ash",
           "sesame","sunflower","oilpalm","castor","safflower","mustard","rapeseed","olive",
           "cashew","coconut","almond","pistachio"
          )
cropp3=c("cotton","sugarcane","tea","coffee","rubber","jute","saffron","pyrethrum","tobacco",
         "sweetpotato","cassava","potato","carrot","turnip","radish",
         "cabbage","tomato","vegetable","broccoli","cauliflower","okra"
         )
cropp4=c( "chilli","pepper","ginger","tumeric","vanilla","lemongrass","cardamom","onion","rosemary","parsley")
if(crop%in%cropp1){suitclass=getSuit(value,1.2,2,2.5,10)}
else if(crop%in%cropp2){suitclass=getSuit(value,1.2,2,3,10)}
else if(crop%in%cropp3){suitclass=getSuit(value,1.2,2,2.8,10)}
else if(crop%in%cropp4){suitclass=getSuit(value,1.2,2.1,2.6,10)}
else {stop("No crop chosen or crop not yet included in database")}
return(suitclass)
}
