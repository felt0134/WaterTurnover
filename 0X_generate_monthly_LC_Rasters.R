
#script to generate and save monthly transpiration rasters for each land cover

#set up file path and land cover name
ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)

ecoregion <- ecoregion_id

#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)

#all months in 2016
file.id<-c(9:21)

for(i in 1:length(file.id)){
  
  file.number <- file.id[i]  
  
  #get data to proper format
  test<-get_land_cover_transp_2(ecoregion=ecoregion,x=file.number)
  
  #save as raster
  
  #first get year and month for filename
  df<-data.frame(et_9km_monthly_2002_2017_length[file.number])
  df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
              '', df$et_9km_monthly_2002_2017_length.file.number.)
  df$id<-gsub('./../../../Data/','', df$id)
  # get year
  year <- substr(df$id, 1, 4)
  # get month
  month <- substr(df$id, 5, 6)
  
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
  writeRaster(test, outfile,overwrite=T)

  print(file.number)
  plot(test)
  
}


#December 2015
# file.id<-c(9)
# for(i in 1:length(file.id)){
#   
#   file.number <- file.id[i]  
#   
#   #get data to proper format
#   test<-get_land_cover_transp_2(ecoregion=ecoregion,x=i)
#   
#   #save as raster
#   
#   #first get year and month for filename
#   df<-data.frame(et_9km_monthly_2002_2017_length[file.number])
#   df
#   df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
#               '', df$et_9km_monthly_2002_2017_length.file.number.)
#   df$id<-gsub('./../../../Data/','', df$id)
#   # get year
#   year <- substr(df$id, 1, 4)
#   # get month
#   month <- substr(df$id, 5, 6)
#   
#   outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
#   writeRaster(test, outfile,overwrite=T)
# 
#   print(file.number)
#   
# }
# 
# 
# 
