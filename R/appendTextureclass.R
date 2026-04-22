appendTextureclass=function(df, method="USDA"){
  soilp=df
  total <- 20
  pb = txtProgressBar(min = 0, max = total, style = 3)
  switch(method,
         USDA={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "USDA.TT", PiC.type  = "t",collapse  = ', '))
         },
         European={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "HYPRES.TT", PiC.type  = "t",collapse  = ', '))
         },
         French={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "FR.AISNE.TT", PiC.type  = "t",collapse  = ', '))
         },
         German={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "DE.BK94.TT", PiC.type  = "t",collapse  = ', '))
         },
         UK={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "UK.SSEW.TT", PiC.type  = "t",collapse  = ', '))
         },
         Australia={soilpj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "AU2.TT", PiC.type  = "t",collapse  = ', '))
         },
         Belgian = {soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BE.TT", PiC.type  = "t",collapse  = ', '))
         },
         Canadian ={ soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "CA.ET.TT", PiC.type  = "t",collapse  = ', '))
         },
         FAO = {soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ISSS.TT", PiC.type  = "t",collapse  = ', '))
         },
         Romania={soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ROM.TT", PiC.type  = "t",collapse  = ', '))
         },
         Polish = {soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "PL.TT", PiC.type  = "t",collapse  = ', '))
         },
         Brazil = {soilj =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BRASIL.TT", PiC.type  = "t",collapse  = ', '))
         },
         stop("No texture classification method chosen")
  )
  for(i in 1:total){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)
  }
  return(soilj)

}
