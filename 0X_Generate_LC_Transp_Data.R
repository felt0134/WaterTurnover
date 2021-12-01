

#Generate Land Cover Transp Data


# generate monthly storage and canopy transpiration data frames for each


# Set file path ------

ncpath_9_km_2015_2017_smap_enhanced <- "./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly/"
et_9km_monthly_2002_2017_length <- dir(ncpath_9_km_2015_2017_smap_enhanced, full.names = T)

#-------------------------------------------------------------------------------
# grassland ------

file.id<-c(1:25)
regions<-c('Grassland') 

#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)
for(i in file.id[10:21]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_transp(region=ecoregion,x=i)
    
    #save as dataframe
    # test<- do.call("rbind", test)
    # test<-get_year_month_column_smap_et_9km(test)
    # test<-test[c(1,2,3,5,6)]
    # #summary(test)
    # 
    # # write to file
    # year<-mean(as.numeric(as.character(test$year)))
    # month<-mean(as.numeric(as.character(test$month)))
    # outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    #write.csv(test, outfile, row.names = F)
    
    #save as raster
    
    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[i])
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', df$et_9km_monthly_2002_2017_length.i.)
    df$id<-gsub('./../../../Data/','', df$id)
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    writeRaster(test, outfile)
    
    print(i)
    

    
    
  }
  
}


#-------------------------------------------------------------------------------
# forest ------
file.id<-c(1:25)
regions<-c('Forest') 

#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)
for(i in file.id[9]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_transp(region=ecoregion,x=i)
    
    #save as raster
    
    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[i])
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', df$et_9km_monthly_2002_2017_length.i.)
    df$id<-gsub('./../../../Data/','', df$id)
    
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    writeRaster(test, outfile)
    
    print(i)
    
  }
  
}

#-------------------------------------------------------------------------------
# tundra ------
file.id<-c(1:25)
regions<-c('Tundra') 

#10:21 = all months in year 2016
#9:10 = December 2015-December 2016 (offset by one)
for(i in file.id[9]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_transp_2(region=ecoregion,x=i)
    
    #save as raster
    
    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[i])
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', df$et_9km_monthly_2002_2017_length.i.)
    df$id<-gsub('./../../../Data/','', df$id)
    
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    writeRaster(test, outfile)
    
    print(i)
    
  }
  
}

#-------------------------------------------------------------------------------
# shrubland ------

file.id<-c(1:25)
regions<-c('Shrubland') 

#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)
for(i in file.id[9]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_transp(region=ecoregion,x=i)
    
    #save as raster
    
    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[i])
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                '', df$et_9km_monthly_2002_2017_length.i.)
    df$id<-gsub('./../../../Data/','', df$id)
    
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    writeRaster(test, outfile)
    
    print(i)
    
  }
  
}

#-------------------------------------------------------------------------------
# cropland ------
file.id<-c(1:25)
regions<-c('Cropland') 

#10:21 = all months in year 2016
#9 = December 2015-December 2016 (offset by one)
for(i in file.id[9]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_transp(region=ecoregion,x=i)
    
    #coverting to DF
    # test<- do.call("rbind", test)
    # test<-get_year_month_column_smap_et_9km(test)
    # test<-test[c(1,2,3,5,6)]
    #summary(test)
    
    # year<-mean(as.numeric(as.character(test$year)))
    # month<-mean(as.numeric(as.character(test$month)))
    # 
    # outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    # write.csv(test, outfile, row.names = F)
    # 
    
    #save as raster
    
    #first get year and month for filename
    df<-data.frame(et_9km_monthly_2002_2017_length[i])
    df$id<-gsub('ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_',
                  '', df$et_9km_monthly_2002_2017_length.i.)
    df$id<-gsub('./../../../Data/','', df$id)
    # get year
    year <- substr(df$id, 1, 4)
    # get month
    month <- substr(df$id, 5, 6)
    
    outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/",ecoregion,"_",year,"_",month,".tif")
    writeRaster(test, outfile)
    
    print(i)
    
  }
  
}


#
test.2<-get_land_cover_transp(region='Cropland',x=19)
plot(test.2$`./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_201609_mean.nc`)
#done

df <- 
  data.frame(rasterToPoints(test.2$`./../../../Data/ET/9km_smap_purdy_2015_2017/9km_monthly//PTJPL_SMAP_ET_201609_mean.nc`))

rasterFromXYZ(df)

