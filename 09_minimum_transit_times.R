# Getting the minimum transit time

source('05_Import_Storage_Transp_Data.R')
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)

#load VWC data-

outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016

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

get_monthly_turnover_VWC <- function(month,land_cover){
  
  
  
  if(month=='january'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('1'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('1'))
    
    
  }else if(month=='february'){
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('2'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('2'))
    
    
  }else if(month=='march'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('3'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('3'))
    
    
  }else if(month=='april'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('4'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('4'))
    
    
  }else if(month=='may'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('5'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('5'))
    
    
  }else if(month=='june'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('6'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('6'))
    
    
  }else if(month=='july'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('7'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('7'))
    
    
  }else if(month=='august'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('8'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('8'))
    
    
  }else if(month=='september'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('9'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('9'))
    
    
  }else if(month=='october'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('10'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('10'))
    
    
  }else if(month=='november'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('11'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('11'))
    
    
  }else if(month=='december'){
    
    
    test.vwc<- test.vwc %>%
      dplyr::filter(month == c('12'))
    
    test.transp <- test.transp %>%
      dplyr::filter(month == c('12'))
    
    
  }
  
  
  # Average across months 
  test.vwc <-aggregate(vwc~x+y,mean,data=test.vwc)
  
  #sum across months
  test.transp<-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp)
  test.transp$canopy_transpiration_mm_m2 <- test.transp$canopy_transpiration_mm_m2/30
  
  # re-grid
  test.vwc <- fix_grid(test.vwc)
  
  #load reference raster
  transit.all.raster<-raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')
  
  #resample to reference raster
  test.vwc <- resample(test.vwc,transit.all.raster)
  
  
  if(land_cover=='grassland'){
    
    test.grassland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.grassland)
    
    # get rid of pixels where T is zero
    test.grassland.cumulative.transp <- test.grassland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.grassland.cumulative.transp$canopy_transpiration_mm_m2 <- test.grassland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.grassland.cumulative.transp)
    rm(test.grassland.cumulative.transp)
    
    
  }else if(land_cover=='forest'){
    
    
    test.forest.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.forest)
    
    # get rid of pixels where T is zero
    test.forest.cumulative.transp <- test.forest.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.forest.cumulative.transp$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.forest.cumulative.transp)
    rm(test.forest.cumulative.transp)
    
    
  }else if(land_cover=='shrubland'){
    
    
    test.shrubland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.shrubland)
    
    # get rid of pixels where T is zero
    test.shrubland.cumulative.transp <- test.shrubland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.shrubland.cumulative.transp$canopy_transpiration_mm_m2 <- test.shrubland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    land_cover_raster<-rasterFromXYZ(test.shrubland.cumulative.transp)
    rm(test.shrubland.cumulative.transp)
    
  }else if(land_cover=='cropland'){
    
    
    
    test.cropland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.cropland)
    
    # get rid of pixels where T is zero
    test.cropland.cumulative.transp <- test.cropland.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.cropland.cumulative.transp$canopy_transpiration_mm_m2 <- test.cropland.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.cropland.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.cropland.cumulative.transp)
    rm(test.cropland.cumulative.transp)
    
    
  }else if(land_cover=='tundra'){
    
    
    test.tundra.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.tundra)
    
    # get rid of pixels where T is zero
    test.tundra.cumulative.transp <- test.tundra.cumulative.transp %>%
      dplyr::filter(canopy_transpiration_mm_m2 > .01)
    
    test.tundra.cumulative.transp$canopy_transpiration_mm_m2 <- test.tundra.cumulative.transp$canopy_transpiration_mm_m2/365
    
    #summary(test.tundra.cumulative.transp)
    
    land_cover_raster<-rasterFromXYZ(test.tundra.cumulative.transp)
    rm(test.tundra.cumulative.transp)
    
    
  }else if(land_cover=='xxx'){}
  
  
  test.transp <- rasterFromXYZ(test.transp)
  test.transp<- resample(test.transp,transit.all.raster)
  
  land_cover_raster <-resample(land_cover_raster,transit.all.raster)
  test.vwc <-mask(test.vwc,land_cover_raster)
  
  test.vwc<-
    merge(rasterToPoints(test.transp),rasterToPoints(test.vwc),
          by=c('x','y'))
  
  test.vwc$turnover <- 
    test.vwc$layer/test.vwc$canopy_transpiration_mm_m2
  
  
  # bound the data by the 1st and 99th percentiles
  #test.vwc <- filter_extremes_turnover(test.vwc)
  
  return(test.vwc)
  
  
  
}

test.grassland.january <- get_monthly_turnover_VWC(month='january',land_cover='grassland')
test.grassland.january <- rasterFromXYZ(test.grassland.january[c(1,2,5)])
plot(test.grassland.january)


#loop it

month_list <- c('january','february','march','april','may','june','july','august',
                'september','october','november','december')


#grasslands ------
months.list.grassland<-list()

for(i in month_list){
  
  
  test_turnover_function<-get_monthly_turnover_VWC(month=i,
                                                   land_cover='grassland')
  
  test_turnover_function$month <- i
  
  months.list.grassland[[i]] <- test_turnover_function
  
  
}

months.grasslands.df <- do.call('rbind',months.list.grassland)
head(months.grasslands.df)
rm(months.list.grassland)

#get sample size for each pixel
# grasslands_sample_size <- aggregate(turnover~x+y,length,data=months.grasslands.df)
# grasslands_sample_size <- rasterFromXYZ(grasslands_sample_size)
# crs(grasslands_sample_size) <- '+proj=longlat +datum=WGS84'
# #plot(test.grassland.sample.size.raster)
# writeRaster(grasslands_sample_size,
#             './../../../Data/Derived_Data/Sample_Sizes/VWC_grasslands_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(grasslands_sample_size)


#get minimum turnover time for each pixel
grasslands_minimum_transit<- aggregate(turnover~x+y,min,data=months.grasslands.df)
grasslands_minimum_transit <- rasterFromXYZ(grasslands_minimum_transit)
crs(grasslands_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(grasslands_minimum_transit)
#summary(grasslands_minimum_transit)
writeRaster(grasslands_minimum_transit,
            './../../../Data/Derived_Data/Turnover/Minimum/VWC_grasslands_minimum_transit.tif',
            overwrite=TRUE)


#-------------------------------------------------------------------------------
#forests------

months.list.forest<-list()

for(i in month_list){
  
  
  test_turnover_function<-get_monthly_turnover_VWC(month=i,
                                                   land_cover='forest')
  
  test_turnover_function$month <- i
  
  months.list.forest[[i]] <- test_turnover_function
  
  
}

months.forests.df <- do.call('rbind',months.list.forest)
head(months.forests.df)
rm(months.list.forest)

#get sample size for each pixel
# forests_sample_size <- aggregate(turnover~x+y,length,data=months.forests.df)
# forests_sample_size <- rasterFromXYZ(forests_sample_size)
# crs(forests_sample_size) <- '+proj=longlat +datum=WGS84'
# #plot(forests_sample_size)
# writeRaster(forests_sample_size,
#             './../../../Data/Derived_Data/Sample_Sizes/VWC_forests_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(forests_sample_size)


#get minimum turnover time for each pixel
forests_minimum_transit<- aggregate(turnover~x+y,min,data=months.forests.df)
forests_minimum_transit <- rasterFromXYZ(forests_minimum_transit)
crs(forests_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(forests_minimum_transit)
#summary(forests_minimum_transit)
writeRaster(forests_minimum_transit,
            './../../../Data/Derived_Data/Turnover/Minimum/VWC_forests_minimum_transit.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------
#shrublands----

months.list.shrubland<-list()

for(i in month_list){
  
  
  test_turnover_function<-get_monthly_turnover_VWC(month=i,
                                                   land_cover='shrubland')
  
  test_turnover_function$month <- i
  
  months.list.shrubland[[i]] <- test_turnover_function
  
  
}

months.shrublands.df <- do.call('rbind',months.list.shrubland)
head(months.shrublands.df)
rm(months.list.shrubland)

#get sample size for each pixel
# shrublands_sample_size <- aggregate(turnover~x+y,length,data=months.shrublands.df)
# shrublands_sample_size <- rasterFromXYZ(shrublands_sample_size)
# crs(shrublands_sample_size) <- '+proj=longlat +datum=WGS84'
# #plot(shrublands_sample_size)
# writeRaster(shrublands_sample_size,
#             './../../../Data/Derived_Data/Sample_Sizes/VWC_shrublands_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(shrublands_sample_size)


#get minimum turnover time for each pixel
shrublands_minimum_transit<- aggregate(turnover~x+y,min,data=months.shrublands.df)
shrublands_minimum_transit <- rasterFromXYZ(shrublands_minimum_transit)
crs(shrublands_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(shrublands_minimum_transit)
#summary(shrublands_minimum_transit)
writeRaster(shrublands_minimum_transit,
            './../../../Data/Derived_Data/Turnover/Minimum/VWC_shrublands_minimum_transit.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------
#croplands----

months.list.cropland<-list()

for(i in month_list){
  
  
  test_turnover_function<-get_monthly_turnover_VWC(month=i,
                                                   land_cover='cropland')
  
  test_turnover_function$month <- i
  
  months.list.cropland[[i]] <- test_turnover_function
  
  
}

months.croplands.df <- do.call('rbind',months.list.cropland)
head(months.croplands.df)

#get sample size for each pixel
croplands_sample_size <- aggregate(turnover~x+y,length,data=months.croplands.df)
croplands_sample_size <- rasterFromXYZ(croplands_sample_size)
crs(croplands_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(croplands_sample_size)
writeRaster(croplands_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_croplands_transit_sample_size.tif',
            overwrite=TRUE)
rm(croplands_sample_size)

#get minimum turnover time for each pixel
croplands_minimum_transit<- aggregate(turnover~x+y,min,data=months.croplands.df)
croplands_minimum_transit <- rasterFromXYZ(croplands_minimum_transit)
crs(croplands_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(croplands_minimum_transit)
#summary(croplands_minimum_transit)
writeRaster(croplands_minimum_transit,
            './../../../Data/Derived_Data/Turnover/Minimum/VWC_croplands_minimum_transit.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------
#tundra-----

months.list.tundra<-list()

for(i in month_list){
  
  
  test_turnover_function<-get_monthly_turnover_VWC(month=i,
                                                   land_cover='tundra')
  
  test_turnover_function$month <- i
  
  months.list.tundra[[i]] <- test_turnover_function
  
  
}

months.tundras.df <- do.call('rbind',months.list.tundra)
head(months.tundras.df)

#get sample size for each pixel
# tundras_sample_size <- aggregate(turnover~x+y,length,data=months.tundras.df)
# tundras_sample_size <- rasterFromXYZ(tundras_sample_size)
# crs(tundras_sample_size) <- '+proj=longlat +datum=WGS84'
# #plot(tundras_sample_size)
# writeRaster(tundras_sample_size,
#             './../../../Data/Derived_Data/Sample_Sizes/VWC_tundras_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(tundras_sample_size)

#get minimum turnover time for each pixel
tundras_minimum_transit<- aggregate(turnover~x+y,min,data=months.tundras.df)
tundras_minimum_transit <- rasterFromXYZ(tundras_minimum_transit)
crs(tundras_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(tundras_minimum_transit)
#summary(tundras_minimum_transit)
writeRaster(tundras_minimum_transit,
            './../../../Data/Derived_Data/Turnover/Minimum/VWC_tundras_minimum_transit.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------
# combine all of them -----

grasslands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_grasslands_minimum_transit.tif')
forests_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_forests_minimum_transit.tif')
shrublands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_shrublands_minimum_transit.tif')
croplands_min <- raster( './../../../Data/Derived_Data/Turnover/Minimum/VWC_croplands_minimum_transit.tif')
tundras_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_tundras_minimum_transit.tif')

global_min <- raster::merge(grasslands_min,forests_min,shrublands_min,croplands_min,tundras_min)
# plot(global_min)
# summary(global_min)

writeRaster(global_min,'./../../../Data/Derived_Data/Turnover/Minimum/VWC_global_minimum_transit.tif',
            overwrite=TRUE)

#stopped here 7/16/2021



