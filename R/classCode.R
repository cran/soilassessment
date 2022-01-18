classCode=function(value,indicator="fertility"){
  if(indicator== "saltclass"){
    classcode=ifelse(value<2,"None",ifelse(value<3,"Saline",ifelse(value<4,"Saline_sodic",ifelse(value<5,"Sodic",ifelse(value<6,"Alkaline","Class not included")))))
  }
  else if(indicator=="saltseverity"){
    #Salinity classes: 1-None; 2-Slight; 7-Very Slight; 3-Moderate; 4-Strong; 5-Very Strong; 6 - Extreme; 8 - Sodic; 9-Saline_sodic; 10-Alkaline
   class0=ifelse(value>4,"Alkaline",ifelse(value>3,"Sodic",ifelse(value>2,"saline_sodic",ifelse(value>1,"saline",ifelse(value>0,"None","Class not included")))))
   class01 =ifelse(value>11,"ExtremeSalinity",ifelse(value>10,"VeryStrongSalinity",ifelse(value>9,"StrongSalinity",ifelse(value>8,"ModerateSalinity",ifelse(value>7,"SlightSalinity", ifelse(value>6,"SlightSalinity",ifelse(value>5,"None",class0)))))))
   class02=ifelse(value>16,"VeryStrongSodicity",ifelse(value>15,"StrongSodicity",ifelse(value>14,"ModerateSodicity",ifelse(value>13,"SlightSodicity",ifelse(value>12,"None",class01)))))
   classcode=ifelse(value>17,"None",ifelse(value>18,"class not included",class02))
   }
  else if(indicator=="texture"){
    classcode=ifelse(value==1,"Cl",ifelse(value==2,"SiCl",ifelse(value==3,"SiClLo",ifelse(value==4,"SiLo",
                                                                                          ifelse(value==5,"SaLo",ifelse(value==6,"Si",ifelse(value==7,"ClLo",ifelse(value==8,"SaCl",
                                                                                                                                                                    ifelse(value==9,"SaClLo",ifelse(value==10,"LoSa",ifelse(value==11,"Lo",ifelse(value==12,"Sa",
                                                                                                                                                                                                                                                  ifelse(value==13,"CSa",ifelse(value==14,"MSa",ifelse(value==15,"FSa",ifelse(value==16,"HCl", "Texture not included"))))))))))))))))
  }
  else if(indicator=="suitability"){
    classcode=ifelse(value==1,"High",ifelse(value==2,"Moderate",ifelse(value==3,"Marginal",ifelse(value==4,"NotsuitableNow",ifelse(value==5,"Notsuitable", "Class not included")))))
  }
  else if(indicator=="fertility"){
    classcode=ifelse(value<1.449,"High",ifelse(value<2.449,"Moderate",ifelse(value<3.449,"Low",ifelse(value<4.5,"VeryLow","Class not included"))))
  }
  else if(indicator=="drainage"){
    #very poorly drained - VPD; poorly drained - PDr, Imperfectly drained - ImDr; Moderately drained - MoDr; well drained - WDr; somewhat excessively drained - Sdr; excessively drained - Exdr
    classcode=ifelse(value==1,"VPD",ifelse(value==2,"PDr",ifelse(value==3,"ImDr",ifelse(value==4,"MoDr",ifelse(value==5,"WDr",ifelse(value==6,"SDr",ifelse(value==7,"ExDr", "Class not included")))))))
  }
  else if(indicator=="erodibility"){classcode=ifelse(value>0.01,"VeryLow",ifelse(value>0.1,"Low",ifelse(value>0.2,"Moderate",ifelse(value>0.6,"High",ifelse(value>0.7,"Extreme", "Class not included")))))
  }
  else if(indicator=="permeability"){classcode=ifelse(value==1,"VerySlow",ifelse(value==2,"Slow",ifelse(value==3,"ModeratelySlow",ifelse(value==4,"Moderate",ifelse(value==5,"ModeratelyRapid",ifelse(value==6,"Rapid",ifelse(value==7,"VeryRapid","Class not included")))))))}
  else if(indicator=="structure"){
    classcode=ifelse(structure==1,"Granular",ifelse(structure==2,"Crumby", ifelse(structure==3,"AngularBlocky",ifelse(structure==4,"Columnar",
                                                                                                                      ifelse(structure==5,"SubAngularBlocky", ifelse(structure==6,"Platty",ifelse(structure==7,"SingleGrain",ifelse(structure==8,"Massive",
                                                                                                                                                                                                                                    ifelse(structure==9,"Prismatic",ifelse(structure==10,"Cloddy","Class not included"))))))))))
  }
  classcode=as.factor(classcode)
  return(classcode)
}
