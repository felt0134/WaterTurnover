# 

# This script produces monthly estimtes of vegetation water storage for each
# land cover type, and uses those monthly estimates to derive the range of
# vegetation water storage experienced across a year to provide an estimate of
#the intra-annual variation in vegetation water storage. The vegetaion water storage
# product used is the vegetation water content data.


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
#head(test.vwc)
rm(test,vwc.list)

#test
# test.srtoage.grassland <- get_monthly_storage_VWC(month='january',land_cover='grassland')
# rm(test.srtoage.grassland)

#list of months to loop through
month_list <- c('january','february','march','april','may','june','july','august',
                'september','october','november','december')

#grasslands ------
months.storage.list.grassland<-list()

for(i in month_list){
  
  
  test_storage_function<-get_monthly_storage_VWC(month=i,
                                                   land_cover='grassland')
  
  test_storage_function$month <- i
  colnames(test_storage_function) <- c('x','y','storage','month')
  
  months.storage.list.grassland[[i]] <- test_storage_function
  
  
}

#convert to dataframe
months.storage.grasslands.df <- do.call('rbind',months.storage.list.grassland)
months.storage.grasslands.df<-data.frame(months.storage.grasslands.df)
rm(months.storage.list.grassland,test_storage_function)
head(months.storage.grasslands.df)


#get sample size for each pixel
grasslands_sample_size <- aggregate(storage~x+y,length,data=months.storage.grasslands.df)
grasslands_sample_size <- rasterFromXYZ(grasslands_sample_size)
crs(grasslands_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(grasslands_sample_size)
writeRaster(grasslands_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_grasslands_storage_sample_size.tif',
            overwrite=TRUE)
rm(grasslands_sample_size)

#get the intra-annual range of storage
grasslands_storage_range<- aggregate(storage~x+y,min_max,data=months.storage.grasslands.df)
grasslands_storage_range <- rasterFromXYZ(grasslands_storage_range)
crs(grasslands_storage_range) <- '+proj=longlat +datum=WGS84'
#plot(grasslands_storage_range)
writeRaster(grasslands_storage_range,
            './../../../Data/Derived_Data/VWC/Range/VWC_grasslands_storage_range.tif',
            overwrite=TRUE)
rm(grasslands_storage_range,months.storage.grasslands.df)

#-------------------------------------------------------------------------------
#forests ------
months.storage.list.forest<-list()

for(i in month_list){
  
  
  test_storage_function<-get_monthly_storage_VWC(month=i,
                                                 land_cover='forest')
  
  test_storage_function$month <- i
  colnames(test_storage_function) <- c('x','y','storage','month')
  
  months.storage.list.forest[[i]] <- test_storage_function
  
  
}

#convert to dataframe
months.storage.forests.df <- do.call('rbind',months.storage.list.forest)
months.storage.forests.df<-data.frame(months.storage.forests.df)
rm(months.storage.list.forest,test_storage_function)
head(months.storage.forests.df)


#get sample size for each pixel
forests_sample_size <- aggregate(storage~x+y,length,data=months.storage.forests.df)
forests_sample_size <- rasterFromXYZ(forests_sample_size)
crs(forests_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(forests_sample_size)
writeRaster(forests_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_forests_storage_sample_size.tif',
            overwrite=TRUE)
rm(forests_sample_size)

#get the intra-annual range of storage
forests_storage_range<- aggregate(storage~x+y,min_max,data=months.storage.forests.df)
forests_storage_range <- rasterFromXYZ(forests_storage_range)
crs(forests_storage_range) <- '+proj=longlat +datum=WGS84'
#plot(forests_storage_range)
writeRaster(forests_storage_range,
            './../../../Data/Derived_Data/VWC/Range/VWC_forests_storage_range.tif',
            overwrite=TRUE)
rm(forests_storage_range,months.storage.forests.df)

#-------------------------------------------------------------------------------
#shrublands ------
months.storage.list.shrubland<-list()

for(i in month_list){
  
  
  test_storage_function<-get_monthly_storage_VWC(month=i,
                                                 land_cover='shrubland')
  
  test_storage_function$month <- i
  colnames(test_storage_function) <- c('x','y','storage','month')
  
  months.storage.list.shrubland[[i]] <- test_storage_function
  
  
}

#convert to dataframe
months.storage.shrublands.df <- do.call('rbind',months.storage.list.shrubland)
months.storage.shrublands.df<-data.frame(months.storage.shrublands.df)
rm(months.storage.list.shrubland,test_storage_function)
head(months.storage.shrublands.df)


#get sample size for each pixel
shrublands_sample_size <- aggregate(storage~x+y,length,data=months.storage.shrublands.df)
shrublands_sample_size <- rasterFromXYZ(shrublands_sample_size)
crs(shrublands_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(shrublands_sample_size)
writeRaster(shrublands_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_shrublands_storage_sample_size.tif',
            overwrite=TRUE)
rm(shrublands_sample_size)

#get the intra-annual range of storage
shrublands_storage_range<- aggregate(storage~x+y,min_max,data=months.storage.shrublands.df)
shrublands_storage_range <- rasterFromXYZ(shrublands_storage_range)
crs(shrublands_storage_range) <- '+proj=longlat +datum=WGS84'
#plot(shrublands_storage_range)
writeRaster(shrublands_storage_range,
            './../../../Data/Derived_Data/VWC/Range/VWC_shrublands_storage_range.tif',
            overwrite=TRUE)
rm(shrublands_storage_range,months.storage.shrublands.df)

#-------------------------------------------------------------------------------
#croplands ------
months.storage.list.cropland<-list()

for(i in month_list){
  
  
  test_storage_function<-get_monthly_storage_VWC(month=i,
                                                 land_cover='cropland')
  
  test_storage_function$month <- i
  colnames(test_storage_function) <- c('x','y','storage','month')
  
  months.storage.list.cropland[[i]] <- test_storage_function
  
  
}

#convert to dataframe
months.storage.croplands.df <- do.call('rbind',months.storage.list.cropland)
months.storage.croplands.df<-data.frame(months.storage.croplands.df)
rm(months.storage.list.cropland,test_storage_function)
head(months.storage.croplands.df)


#get sample size for each pixel
croplands_sample_size <- aggregate(storage~x+y,length,data=months.storage.croplands.df)
croplands_sample_size <- rasterFromXYZ(croplands_sample_size)
crs(croplands_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(croplands_sample_size)
writeRaster(croplands_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_croplands_storage_sample_size.tif',
            overwrite=TRUE)
rm(croplands_sample_size)

#get the intra-annual range of storage
croplands_storage_range<- aggregate(storage~x+y,min_max,data=months.storage.croplands.df)
croplands_storage_range <- rasterFromXYZ(croplands_storage_range)
crs(croplands_storage_range) <- '+proj=longlat +datum=WGS84'
#plot(croplands_storage_range)
writeRaster(croplands_storage_range,
            './../../../Data/Derived_Data/VWC/Range/VWC_croplands_storage_range.tif',
            overwrite=TRUE)
rm(croplands_storage_range,months.storage.croplands.df)

#-------------------------------------------------------------------------------
#tundras ------
months.storage.list.tundra<-list()

for(i in month_list){
  
  
  test_storage_function<-get_monthly_storage_VWC(month=i,
                                                 land_cover='tundra')
  
  test_storage_function$month <- i
  colnames(test_storage_function) <- c('x','y','storage','month')
  
  months.storage.list.tundra[[i]] <- test_storage_function
  
  
}

#convert to dataframe
months.storage.tundras.df <- do.call('rbind',months.storage.list.tundra)
months.storage.tundras.df<-data.frame(months.storage.tundras.df)
rm(months.storage.list.tundra,test_storage_function)
head(months.storage.tundras.df)


#get sample size for each pixel
tundras_sample_size <- aggregate(storage~x+y,length,data=months.storage.tundras.df)
tundras_sample_size <- rasterFromXYZ(tundras_sample_size)
crs(tundras_sample_size) <- '+proj=longlat +datum=WGS84'
#plot(tundras_sample_size)
writeRaster(tundras_sample_size,
            './../../../Data/Derived_Data/Sample_Sizes/VWC_tundras_storage_sample_size.tif',
            overwrite=TRUE)
rm(tundras_sample_size)

#get the intra-annual range of storage
tundras_storage_range<- aggregate(storage~x+y,min_max,data=months.storage.tundras.df)
tundras_storage_range <- rasterFromXYZ(tundras_storage_range)
crs(tundras_storage_range) <- '+proj=longlat +datum=WGS84'
#plot(tundras_storage_range)
writeRaster(tundras_storage_range,
            './../../../Data/Derived_Data/VWC/Range/VWC_tundras_storage_range.tif',
            overwrite=TRUE)
rm(tundras_storage_range)

#-------------------------------------------------------------------------------
# combine all to produce global rasters -------

# range
grasslands_range <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_grasslands_storage_range.tif')
forests_range <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_forests_storage_range.tif')
shrublands_range <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_shrublands_storage_range.tif')
croplands_range <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_croplands_storage_range.tif')
tundras_range <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_tundras_storage_range.tif')

global_range <- raster::merge(grasslands_range,forests_range,shrublands_range,croplands_range,tundras_range)
# plot(global_range)
# summary(global_range)

writeRaster(global_range,'./../../../Data/Derived_Data/VWC/Range/VWC_global_range_storage.tif',
            overwrite=TRUE)


#sample size
grasslands_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_grasslands_storage_sample_size.tif')
forests_ss<- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_forests_storage_sample_size.tif')
shrublands_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_shrublands_storage_sample_size.tif')
croplands_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_croplands_storage_sample_size.tif')
tundras_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_tundras_storage_sample_size.tif')


global_ss <- raster::merge(grasslands_ss,forests_ss,shrublands_ss,croplands_ss,tundras_ss)
# plot(global_ss)
# summary(global_range)

writeRaster(global_ss,'./../../../Data/Derived_Data/Sample_Sizes/VWC_global_storage_sample_size.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------