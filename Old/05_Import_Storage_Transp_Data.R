
# import the derived storage and transp data

# import grasslands data ------

regions<-c('Grasslands')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/wc_5/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    

    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.grassland<- do.call("rbind", region.list)
rm(region.list,test)

#get sample size for each pixel (proportion of 12)
# test.grassland.sample.size <- aggregate(water_storage_mm_m2~x+y,length,data=test.grassland)
# test.grassland.sample.size$water_storage_mm_m2 <- (test.grassland.sample.size$water_storage_mm_m2)/12
# summary(test.grassland.sample.size)
# 
# #turn into raster and save
# test.grassland.sample.size.raster <- rasterFromXYZ(test.grassland.sample.size)
# crs(test.grassland.sample.size.raster) <- '+proj=longlat +datum=WGS84'
# #plot(test.grassland.sample.size.raster)
# writeRaster(test.grassland.sample.size.raster,
#             './../../../Data/Derived_Data/Sample_Sizes/tissue_density_grasslands_transit_sample_size.tif',
#             overwrite=TRUE)
#rm(test.grassland.sample.size)

#-------------------------------------------------------------------------------
# import forests data ------


regions<-c('Forest')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',regions,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){

    test<-fread(j)
    region.list[[j]] <- test
    
    
  }}

#make into data frame
test.forest<- do.call("rbind", region.list)
rm(region.list,test)

#get sample size for each pixel (proportion of 12)
test.forest.sample.size <- aggregate(water_storage_mm_m2~x+y,length,data=test.forest)
test.forest.sample.size$water_storage_mm_m2 <- (test.forest.sample.size$water_storage_mm_m2)/12
summary(test.forest.sample.size)

#turn into raster and save
# test.forest.sample.size.raster <- rasterFromXYZ(test.forest.sample.size)
# crs(test.forest.sample.size.raster) <- '+proj=longlat +datum=WGS84'
# #plot(test.forest.sample.size.raster)
# writeRaster(test.forest.sample.size.raster,
#             './../../../Data/Derived_Data/Sample_Sizes/tissue_density_forest_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(test.forest.sample.size)




#-------------------------------------------------------------------------------
# import tundra data ------

regions<-c('Tundra')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    
 
    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.tundra<- do.call("rbind", region.list)
rm(region.list,test)

#get sample size for each pixel (proportion of 12)
# test.tundra.sample.size <- aggregate(water_storage_mm_m2~x+y,length,data=test.tundra)
# test.tundra.sample.size$water_storage_mm_m2 <- (test.grassland.sample.size$water_storage_mm_m2)/12
# summary(test.tundra.sample.size)
# 
# #turn into raster and save
# test.tundra.sample.size.raster <- rasterFromXYZ(test.tundra.sample.size)
# crs(test.tundra.sample.size.raster) <- '+proj=longlat +datum=WGS84'
# #plot(test.tundra.sample.size.raster)
# writeRaster(test.tundra.sample.size.raster,
#             './../../../Data/Derived_Data/Sample_Sizes/tissue_density_tundra_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(test.tundra.sample.size)


#-------------------------------------------------------------------------------
# import shrubland data ------

regions<-c('Shrubland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    

    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.shrubland<- do.call("rbind", region.list)
rm(region.list,test)

#get sample size for each pixel (proportion of 12)
# test.shrubland.sample.size <- aggregate(water_storage_mm_m2~x+y,length,data=test.shrubland)
# test.shrubland.sample.size$water_storage_mm_m2 <- (test.shrubland.sample.size$water_storage_mm_m2)/12
# summary(test.shrubland.sample.size)
# 
# #turn into raster and save
# test.shrubland.sample.size.raster <- rasterFromXYZ(test.shrubland.sample.size)
# crs(test.shrubland.sample.size.raster) <- '+proj=longlat +datum=WGS84'
# #plot(test.shrubland.sample.size.raster)
# writeRaster(test.shrubland.sample.size.raster,
#             './../../../Data/Derived_Data/Sample_Sizes/tissue_density_shrubland_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(test.shrubland.sample.size)

#-------------------------------------------------------------------------------
# import cropland data ------
regions<-c('Cropland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    
    
    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.cropland<- do.call("rbind", region.list)
rm(region.list,test)

#get sample size for each pixel (proportion of 12)
# test.cropland.sample.size <- aggregate(water_storage_mm_m2~x+y,length,data=test.cropland)
# test.cropland.sample.size$water_storage_mm_m2 <- (test.cropland.sample.size$water_storage_mm_m2)/12
# summary(test.cropland.sample.size)
# 
# #turn into raster and save
# test.cropland.sample.size.raster <- rasterFromXYZ(test.cropland.sample.size)
# crs(test.cropland.sample.size.raster) <- '+proj=longlat +datum=WGS84'
# #plot(test.cropland.sample.size.raster)
# writeRaster(test.cropland.sample.size.raster,
#             './../../../Data/Derived_Data/Sample_Sizes/tissue_density_cropland_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(test.cropland.sample.size)

#-------------------------------------------------------------------------------
