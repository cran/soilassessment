permeabilityClass=function(texture){
  #permeability classes:1-VerySlow, 2-Slow; 3-ModerateSlow; 4-Moderate; 5-ModerateRapid; 6-Rapid; 7-VeryRapid
  permclass=ifelse(texture==1,1,ifelse(texture==8,2,ifelse(texture==2,2,ifelse(texture==7,3,ifelse(texture==9,3,ifelse(texture==3,3,
            ifelse(texture==15,4,ifelse(texture==11,4,ifelse(texture==4,4,ifelse(texture==6,4,ifelse(texture==5,5,ifelse(texture==10,6,
            ifelse(texture==12,6,ifelse(texture==13,7,ifelse(texture==16,1,0)))))))))))))))
 return(permclass)
}
