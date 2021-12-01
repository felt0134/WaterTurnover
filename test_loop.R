


#load VWC data

outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,3,4,17)] #remove december 2016


vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  test.vwc<-aggregate(vwc~x+y,mean,data=test)
  test.vwc$month <- j
  test.vwc$month <- gsub('./../../../Data/Derived_data/VWC//vwc_2016_','',test.vwc$month)
  test.vwc$month <- gsub('./../../../Data/Derived_data/VWC//vwc_2015_','',test.vwc$month)
  test.vwc$month <- gsub('.csv','',test.vwc$month)
  #test.vwc$month <- gsub('.csv','',test.vwc$month)
  test.vwc$month <-as.numeric(as.character(test.vwc$month))
  
  vwc.list[[j]] <- test.vwc
  
  
  
}

test.vwc<-do.call('rbind',vwc.list)
rownames(test.vwc) = NULL
# head(test.vwc)
# unique(test.vwc$month)


seasons <- c('december_february','march_may','june_august','september_november')

#grasslands----
seasons.list<-list()

for(i in seasons){
  
  test_turnover_function<-get_seasonal_turnover_2(season=i)
  
  test_turnover_function$season <- i
  
  seasons.list[[i]] <- test_turnover_function
  
  
}

seasons.df <- do.call('rbind',seasons.list)
rm(seasons.list)

#save the unfiltered files 

#winter
winter_unfiltered <- subset(seasons.df,season=='december_february')

#turnover
winter_transit<- rasterFromXYZ(winter_unfiltered[c(1,2,5)])
crs(winter_transit) <- '+proj=longlat +datum=WGS84'
writeRaster(winter_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)

#storage
winter_storage <- rasterFromXYZ(winter_unfiltered[c(1,2,4)])
crs(winter_storage) <- '+proj=longlat +datum=WGS84'
writeRaster(winter_storage,
            paste0('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)
rm(winter_storage,winter_transit,winter_unfiltered)



#spring
spring_unfiltered <- subset(seasons.df,season=='march_may')

#turnover
spring_transit <- rasterFromXYZ(spring_unfiltered[c(1,2,5)])
crs(spring_transit) <- '+proj=longlat +datum=WGS84'
writeRaster(spring_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)

#storage
spring_storage <- rasterFromXYZ(spring_unfiltered[c(1,2,4)])
crs(spring_storage) <- '+proj=longlat +datum=WGS84'
writeRaster(spring_storage,
            paste0('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)
rm(spring_storage,spring_transit,spring_unfiltered)


#summer
summer_unfiltered <- subset(seasons.df,season=='june_august')

#turnover
summer_transit <- rasterFromXYZ(summer_unfiltered[c(1,2,5)])
crs(summer_transit) <- '+proj=longlat +datum=WGS84'
writeRaster(summer_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)


#storage
summer_storage <- rasterFromXYZ(summer_unfiltered[c(1,2,4)])
crs(summer_storage) <- '+proj=longlat +datum=WGS84'
writeRaster(summer_storage,
            paste0('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)
rm(summer_storage,summer_transit,summer_unfiltered)


#fall
fall_unfiltered <- subset(seasons.df,season=='september_november')

#turnover
fall_transit <- rasterFromXYZ(fall_unfiltered[c(1,2,5)])
crs(fall_transit) <- '+proj=longlat +datum=WGS84'
writeRaster(fall_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)

#storage
fall_storage <- rasterFromXYZ(fall_unfiltered[c(1,2,4)])
crs(fall_storage) <- '+proj=longlat +datum=WGS84'
writeRaster(fall_storage,
            paste0('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)
#rm(fall_storage,fall_transit,fall_unfiltered)

