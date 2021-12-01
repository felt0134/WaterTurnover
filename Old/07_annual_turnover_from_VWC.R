#

#This script produces annualized (pixel averaged) estimates of transit time and
#vegetation water storage. Water storage is derived from the vegetation water content
# product.

#prep
source('05_Import_Storage_Transp_Data.R')
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)

#grasslands -----

test.grassland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.grassland)
# head(test.grassland.cumulative.transp)
# str(test.grassland.cumulative.transp)

# get rid of pixels where T is zero
test.grassland.cumulative.transp <- test.grassland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.grassland.cumulative.transp$canopy_transpiration_mm_m2 <- test.grassland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.grassland.cumulative.transp)

grasslandraster<-rasterFromXYZ(test.grassland.cumulative.transp)
crs(grasslandraster)
rm(test.grassland.cumulative.transp)


#load in grassland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,grasslandraster)
vwc.grassland <- mask(test.vwc,grasslandraster)
rm(test.vwc)
# vwc.grassland <- resample(vwc.grassland,grasslandraster)
# plot(vwc.grassland)
# plot(grasslandraster)

#try to stack them
stack.test<- raster::stack(vwc.grassland,grasslandraster)
#plot(stack.test)

#create and save annual storage
# average_vwc_grasslands <- stack.test$layer
# crs(average_vwc_grasslands) <- '+proj=longlat +datum=WGS84'
# #plot(average_vwc_grasslands)
# writeRaster(average_vwc_grasslands,
#             './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.grasslands <- stack.test$new
transit.grasslands.df <- as.data.frame(rasterToPoints(transit.grasslands))
#summary(transit.grasslands.df)
#hist(transit.grasslands.df$new)

#save the unfiltered turnover raster 
# transit.grasslands.df.unfiltered.raster <- rasterFromXYZ(transit.grasslands.df)
# crs(transit.grasslands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.grasslands.df.unfiltered.raster)
# writeRaster(transit.grasslands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')


#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.grasslands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.grasslands.df$new,probs=c(0.05)))

transit.grasslands.df <- transit.grasslands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.grasslands.df$Cover <- 'grasslands'

hist(transit.grasslands.df$new)
summary(transit.grasslands.df$new)

#change back to raster to plot
grassland.transit.annual <- rasterFromXYZ(transit.grasslands.df[c(1,2,3)])
plot(grassland.transit.annual)
rm(stack.test,transit.grasslands)

#done 


#-------------------------------------------------------------------------------
#forests ------

#compare
# test.forest.cumulative.transp.compare <- aggregate(canopy_transpiration_mm_m2~x+y,mean,data=test.forest)
# test.forest.cumulative.transp.compare$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp.compare$canopy_transpiration_mm_m2/30.5
# summary(test.forest.cumulative.transp.compare)
# test.forest.cumulative.transp.compare.2 <- aggregate(canopy_transpiration_mm_m2~x+y,mean,data=test.forest)
# test.forest.cumulative.transp.compare.2$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp.compare.2$canopy_transpiration_mm_m2/365
# summary(test.forest.cumulative.transp.compare.2)
# monthly average approach is higher than annual sum approach

test.forest.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.forest)
# head(test.forest.cumulative.transp)
# str(test.forest.cumulative.transp)

# get rid of pixels where T is zero
test.forest.cumulative.transp <- test.forest.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.forest.cumulative.transp$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.forest.cumulative.transp)

forestraster<-rasterFromXYZ(test.forest.cumulative.transp)
rm(test.forest.cumulative.transp)


#load in forest VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,forestraster)
vwc.forest <- mask(test.vwc,forestraster)
rm(test.vwc)
# vwc.forest <- resample(vwc.forest,forestraster)
# plot(vwc.forest)
# plot(forestraster)

#try to stack them
stack.test<- raster::stack(vwc.forest,forestraster)
#plot(stack.test)

#create and save annual storage
# average_vwc_forests <- stack.test$layer
# crs(average_vwc_forests) <- '+proj=longlat +datum=WGS84'
# #plot(average_vwc_forests)
# writeRaster(average_vwc_forests,
#             './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.forests <- stack.test$new
transit.forests.df <- as.data.frame(rasterToPoints(transit.forests))
#summary(transit.forests.df)
#hist(transit.forests.df$new)

#save the unfiltered raster
transit.forests.df.unfiltered.raster <- rasterFromXYZ(transit.forests.df)
crs(transit.forests.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
plot(transit.forests.df.unfiltered.raster)
writeRaster(transit.forests.df.unfiltered.raster,
            './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')

#STOPPED HERE 07/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.forests.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.forests.df$new,probs=c(0.05)))

transit.forests.df <- transit.forests.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.forests.df$Cover <- 'forests'

#change back to raster to plot
forest.transit.annual <- rasterFromXYZ(transit.forests.df[c(1,2,3)])
plot(forest.transit.annual)
rm(stack.test,transit.forests)

#-------------------------------------------------------------------------------
#shrublands ------

test.shrubland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.shrubland)
# head(test.shrubland.cumulative.transp)
# str(test.shrubland.cumulative.transp)

# get rid of pixels where T is zero
test.shrubland.cumulative.transp <- test.shrubland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.shrubland.cumulative.transp$canopy_transpiration_mm_m2 <- test.shrubland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.shrubland.cumulative.transp)

shrublandraster<-rasterFromXYZ(test.shrubland.cumulative.transp)
rm(test.shrubland.cumulative.transp)


#load in shrubland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,shrublandraster)
vwc.shrubland <- mask(test.vwc,shrublandraster)
rm(test.vwc)
# vwc.shrubland <- resample(vwc.shrubland,shrublandraster)
# plot(vwc.shrubland)
# plot(shrublandraster)

#try to stack them
stack.test<- raster::stack(vwc.shrubland,shrublandraster)
#plot(stack.test)

#create and save annual storage
# average_vwc_shrublands <- stack.test$layer
# crs(average_vwc_shrublands) <- '+proj=longlat +datum=WGS84'
# #plot(average_vwc_shrublands)
# writeRaster(average_vwc_shrublands,
#             './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.shrublands <- stack.test$new
transit.shrublands.df <- as.data.frame(rasterToPoints(transit.shrublands))
#summary(transit.shrublands.df)
#hist(transit.shrublands.df$new)

# save the unfiltered raster
# transit.shrublands.df.unfiltered.raster <- rasterFromXYZ(transit.shrublands.df)
# crs(transit.shrublands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.shrublands.df.unfiltered.raster)
# writeRaster(transit.shrublands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')

#STOPPED HERE 7/15/2021


# filter out extreme values
high<-as.numeric(quantile(transit.shrublands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.shrublands.df$new,probs=c(0.05)))

transit.shrublands.df <- transit.shrublands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.shrublands.df$Cover <- 'shrublands'

#change back to raster to plot
shrubland.transit.annual <- rasterFromXYZ(transit.shrublands.df[c(1,2,3)])
plot(shrubland.transit.annual)
rm(stack.test,transit.shrublands)



#-------------------------------------------------------------------------------
#tundras ------
test.tundra.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.tundra)
# head(test.tundra.cumulative.transp)
# str(test.tundra.cumulative.transp)

# get rid of pixels where T is zero
test.tundra.cumulative.transp <- test.tundra.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.tundra.cumulative.transp$canopy_transpiration_mm_m2 <- test.tundra.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.tundra.cumulative.transp)

tundraraster<-rasterFromXYZ(test.tundra.cumulative.transp)
crs(tundraraster) <- '+proj=longlat +datum=WGS84'
rm(test.tundra.cumulative.transp)


#load in tundra VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,tundraraster)
vwc.tundra <- mask(test.vwc,tundraraster)
rm(test.vwc)
# vwc.tundra <- resample(vwc.tundra,tundraraster)
# plot(vwc.tundra)
# plot(tundraraster)

#try to stack them
stack.test<- raster::stack(vwc.tundra,tundraraster)
#plot(stack.test)

#create and save annual storage
# average_vwc_tundras <- stack.test$layer
# crs(average_vwc_tundras) <- '+proj=longlat +datum=WGS84'
# #plot(average_vwc_tundras)
# writeRaster(average_vwc_tundras,
#             './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2

transit.tundras <- stack.test$new
transit.tundras.df <- as.data.frame(rasterToPoints(transit.tundras))
#summary(transit.tundras.df)
#hist(transit.tundras.df$new)

#save the unfiltered raster 
transit.tundras.df.unfiltered.raster <- rasterFromXYZ(transit.tundras.df)
crs(transit.tundras.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
plot(transit.tundras.df.unfiltered.raster)
writeRaster(transit.tundras.df.unfiltered.raster,
            './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')

#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.tundras.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.tundras.df$new,probs=c(0.05)))

transit.tundras.df <- transit.tundras.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.tundras.df$Cover <- 'tundras'

#change back to raster to plot
tundra.transit.annual <- rasterFromXYZ(transit.tundras.df[c(1,2,3)])
plot(tundra.transit.annual)
rm(stack.test,transit.tundras)

#-------------------------------------------------------------------------------
#croplands -----

test.cropland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.cropland)
# head(test.cropland.cumulative.transp)
# str(test.cropland.cumulative.transp)

# get rid of pixels where T is zero
test.cropland.cumulative.transp <- test.cropland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.cropland.cumulative.transp$canopy_transpiration_mm_m2 <- test.cropland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.cropland.cumulative.transp)

croplandraster<-rasterFromXYZ(test.cropland.cumulative.transp)
rm(test.cropland.cumulative.transp)


#load in cropland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,croplandraster)
vwc.cropland <- mask(test.vwc,croplandraster)
rm(test.vwc)

#try to stack them
stack.test<- raster::stack(vwc.cropland,croplandraster)
#plot(stack.test)

#create and save annual storage
# average_vwc_croplands <- stack.test$layer
# crs(average_vwc_croplands) <- '+proj=longlat +datum=WGS84'
# #plot(average_vwc_croplands)
# writeRaster(average_vwc_croplands,
#             './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.croplands <- stack.test$new
transit.croplands.df <- as.data.frame(rasterToPoints(transit.croplands))
#summary(transit.croplands.df)
#hist(transit.croplands.df$new)

#save the unfiltered raster 
# transit.croplands.df.unfiltered.raster <- rasterFromXYZ(transit.croplands.df)
# crs(transit.croplands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.croplands.df.unfiltered.raster)
# writeRaster(transit.croplands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_croplands_unfiltered.tif')

#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.croplands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.croplands.df$new,probs=c(0.05)))

transit.croplands.df <- transit.croplands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.croplands.df$Cover <- 'croplands'

hist(transit.croplands.df$new)
summary(transit.croplands.df$new)

#change back to raster to plot
cropland.transit.annual <- rasterFromXYZ(transit.croplands.df[c(1,2,3)])
plot(cropland.transit.annual)
rm(stack.test,transit.croplands)

#done 


#-------------------------------------------------------------------------------
#import the unfiltered transit files, combine them all and save global raster-----

#import raster
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
summary(rasterToPoints(grasslands_unfiltered))
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
summary(forests_unfiltered)
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
summary(shrublands_unfiltered)
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
summary(tundras_unfiltered)
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_croplands_unfiltered.tif')
summary(croplands_unfiltered)


global_raster_unfiltered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                            shrublands_unfiltered,tundras_unfiltered,
                            croplands_unfiltered)
summary(global_raster_unfiltered)
#6.1 days

# writeRaster(global_raster_unfiltered,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')



#-------------------------------------------------------------------------------
#import the unfiltered storage files, combine them all, and save global raster-----

#import raster
grasslands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')


global_storage_raster_unfiltered <- raster::merge(grasslands_storage_unfiltered,
                                                  forests_storage_unfiltered,
                                          shrublands_storage_unfiltered,
                                          tundras_storage_unfiltered,
                                          croplands_storage_unfiltered)
summary(global_storage_raster_unfiltered)
plot(global_storage_raster_unfiltered)

writeRaster(global_storage_raster_unfiltered,
            './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')


