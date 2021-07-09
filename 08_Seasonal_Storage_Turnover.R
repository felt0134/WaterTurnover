
#load reference raster
transit.all.raster<-raster('./../../../Data/Derived_data/VWC/Global/global_transit_2016.tif')

#load VWC data

outfile <- './../../../Data/Derived_data/VWC/'
ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  test.vwc<-aggregate(vwc~x+y,mean,data=test)
  test.vwc$month <- j
  test.vwc$month <- gsub('./../../../Data/Derived_data/VWC//vwc_2016_','',test.vwc$month)
  test.vwc$month <- gsub('.csv','',test.vwc$month)
  test.vwc$month <-as.numeric(as.character(test.vwc$month))
  
  vwc.list[[j]] <- test.vwc
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
#test.vwc$month <- as.numeric(as.character(test.vwc$month))
head(test.vwc)
rm(vwc.list,test)

# Filter to Jan-March
test.vwc.jan.march <- test.vwc %>%
  dplyr::filter(month < 4)

# head(test.vwc.jan.march)
# unique(test.vwc.jan.march$month)
test.vwc.jan.march<-aggregate(vwc~x+y,mean,data=test.vwc.jan.march)

#re-grid to normal
test.vwc.jan.march<-fix_grid(test.vwc.jan.march)
#plot(test.vwc.jan.march)

#resample to reference raster
test.vwc.jan.march <- resample(test.vwc.jan.march,transit.all.raster)
# plot(test.vwc.jan.march)

#save unmasked raster
# writeRaster(test.vwc.jan.march,'./../../../Data/Derived_data/VWC/Seasonal/Jan_March.tif')

# mask raster to different land cover types
test.vwc.jan.march.masked <- mask(test.vwc.jan.march,transit.all.raster)
plot(test.vwc.jan.march.masked)


# Filter to April-June
test.vwc.april.june <- test.vwc %>%
  dplyr::filter(month < 7) %>%
  dplyr::filter(month > 3)

# Average across months 
test.vwc.april.june <-aggregate(vwc~x+y,mean,data=test.vwc.april.june)
head(test.vwc.april.june)

# re-grid
test.vwc.april.june<-fix_grid(test.vwc.april.june)

#resample to reference raster
test.vwc.april.june <- resample(test.vwc.april.june,transit.all.raster)
#plot(test.vwc.april.june)

#save unmasked raster
# writeRaster(test.vwc.april.june,'./../../../Data/Derived_data/VWC/Seasonal/April_June.tif')

# mask raster to different land cover types
test.vwc.april.june.masked <- mask(test.vwc.april.june,transit.all.raster)
#plot(test.vwc.april.june.masked)

# Filter to July-September
test.vwc.july.sep <- test.vwc %>%
  dplyr::filter(month < 10) %>%
  dplyr::filter(month > 6)

# Average across months 
test.vwc.july.sep <-aggregate(vwc~x+y,mean,data=test.vwc.july.sep)
head(test.vwc.july.sep)

# re-grid
test.vwc.july.sep<-fix_grid(test.vwc.july.sep)

#resample to reference raster
test.vwc.july.sep <- resample(test.vwc.july.sep,transit.all.raster)
#plot(test.vwc.july.sep)

#save unmasked raster
# writeRaster(test.vwc.july.sep,'./../../../Data/Derived_data/VWC/Seasonal/July_September.tif')

# mask raster to different land cover types
test.vwc.july.sep.masked <- mask(test.vwc.july.sep,transit.all.raster)
#plot(test.vwc.july.sep.masked)

# Filter to October December
test.vwc.oct_dec <- test.vwc %>%
  dplyr::filter(month > 9) 

# Average across months 
test.vwc.oct_dec  <-aggregate(vwc~x+y,mean,data=test.vwc.oct_dec)
head(test.vwc.oct_dec )

# re-grid
test.vwc.oct_dec <-fix_grid(test.vwc.oct_dec)

#resample to reference raster
test.vwc.oct_dec <- resample(test.vwc.oct_dec,transit.all.raster)
#plot(test.vwc.oct_dec)

#save unmasked raster
# writeRaster(test.vwc.oct_dec,'./../../../Data/Derived_data/VWC/Seasonal/October_December.tif')

# mask raster to different land cover types
test.vwc.oct_dec.masked <- mask(test.vwc.oct_dec,transit.all.raster)
#plot(test.vwc.oct_dec.masked)


# get T data

source('05_Import_Storage_Transp_Data.R')
head(test.tundra)
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)
head(test.transp)

# Filter to January-March
test.transp.jan.march <- test.transp %>%
  dplyr::filter(month < 4) 
#unique(test.transp.jan.march$month)

# Average across months 
test.transp.jan.march <-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp.jan.march)
#head(test.transp.jan.march)

#get to daily transpiration
test.transp.jan.march$canopy_transpiration_mm_m2 <- test.transp.jan.march$canopy_transpiration_mm_m2/90

test.transp.jan.march <- rasterFromXYZ(test.transp.jan.march)

test.transp.jan.march <- resample(test.transp.jan.march,transit.all.raster)

#save raster
# writeRaster(test.transp.jan.march,'./../../../Data/Derived_data/ET/Seasonal/January_March_Transp.tif',
#             overwrite=TRUE)

#plot(test.transp.jan.march)

#combine and get turnover

merge.transp.storage.jan.march<-
  merge(rasterToPoints(test.transp.jan.march),rasterToPoints(test.vwc.jan.march),
                  by=c('x','y'))

#remove zeros to remove inf values. 
merge.transp.storage.jan.march <-merge.transp.storage.jan.march %>%
  dplyr::filter(layer>0.01) %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0.01)

merge.transp.storage.jan.march$turnover <- 
  merge.transp.storage.jan.march$layer/merge.transp.storage.jan.march$canopy_transpiration_mm_m2

# hist(merge.transp.storage.jan.march$turnover)
# 
# head(merge.transp.storage.jan.march)
# summary(merge.transp.storage.jan.march)

# filter out extreme values
jan.march.turnover.filtered <- filter_extremes_turnover(merge.transp.storage.jan.march)

summary(jan.march.turnover.filtered)
hist(jan.march.turnover.filtered$turnover)

jan.march.turnover.filtered<-jan.march.turnover.filtered[c(1,2,5)]
jan.march.turnover.filtered.raster<-rasterFromXYZ(jan.march.turnover.filtered)
crs(jan.march.turnover.filtered.raster) <-'+proj=longlat +datum=WGS84 +no_defs'
plot(jan.march.turnover.filtered.raster,main='jan-march')

#save raster
# writeRaster(jan.march.turnover.filtered.raster,
#             './../../../Data/Derived_data/Turnover/Seasonal/January_March_Turnover.tif',
#             overwrite=TRUE)

#give season ID for later merge
jan.march.turnover.filtered$season <-'jan_march'


#
#


#get April-June



# Filter to April-June
test.transp.april.june <- test.transp %>%
  dplyr::filter(month > 4) %>%
  dplyr::filter(month < 6)
#unique(test.transp.april.june$month)

# sum across months 
test.transp.april.june <-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp.april.june)
#head(test.transp.april.june)

#get to daily transpiration
test.transp.april.june$canopy_transpiration_mm_m2 <- test.transp.april.june$canopy_transpiration_mm_m2/90

test.transp.april.june <- rasterFromXYZ(test.transp.april.june)

test.transp.april.june <- resample(test.transp.april.june,transit.all.raster)

#save raster
# writeRaster(test.transp.april.june,'./../../../Data/Derived_data/ET/Seasonal/April_June_Transp.tif',
#             overwrite=TRUE)

#plot(test.transp.april.june)

#combine and get turnover

merge.transp.storage.april.june<-
  merge(rasterToPoints(test.transp.april.june),rasterToPoints(test.vwc.april.june),
        by=c('x','y'))

#remove zeros to remove inf values. 
merge.transp.storage.april.june <-merge.transp.storage.april.june %>%
  dplyr::filter(layer>0.01) %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0.01)

merge.transp.storage.april.june$turnover <- 
  merge.transp.storage.april.june$layer/merge.transp.storage.april.june$canopy_transpiration_mm_m2

# hist(merge.transp.storage.april.june$turnover)
# 
# head(merge.transp.storage.april.june)
# summary(merge.transp.storage.april.june)

# filter out extreme values
april.june.turnover.filtered <- filter_extremes_turnover(merge.transp.storage.april.june)

summary(april.june.turnover.filtered)
hist(april.june.turnover.filtered$turnover)

april.june.turnover.filtered<-april.june.turnover.filtered[c(1,2,5)]
april.june.turnover.filtered.raster<-rasterFromXYZ(april.june.turnover.filtered)
crs(april.june.turnover.filtered.raster) <-'+proj=longlat +datum=WGS84 +no_defs'
plot(april.june.turnover.filtered.raster,main='april-june')

#save raster
# writeRaster(april.june.turnover.filtered.raster,
#             './../../../Data/Derived_data/Turnover/Seasonal/April_June_Turnover.tif',
#             overwrite=TRUE)

#give season ID for later merge
april.june.turnover.filtered$season <-'april_june'


#
#



#now do July-September
test.transp.july.sep <- test.transp %>%
  dplyr::filter(month > 6) %>%
  dplyr::filter(month < 10)
#unique(test.transp.july.sep$month)

# Average across months 
test.transp.july.sep <-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp.july.sep)
#head(test.transp.july.sep)

#get to daily transpiration
test.transp.july.sep$canopy_transpiration_mm_m2 <- test.transp.july.sep$canopy_transpiration_mm_m2/90

test.transp.july.sep <- rasterFromXYZ(test.transp.july.sep)

test.transp.july.sep <- resample(test.transp.july.sep,transit.all.raster)

#save raster
# writeRaster(test.transp.july.sep,'./../../../Data/Derived_data/ET/Seasonal/July_September_Transp.tif',
#             overwrite=TRUE)

#plot(test.transp.july.sep)

#combine and get turnover

merge.transp.storage.july.sep<-
  merge(rasterToPoints(test.transp.july.sep),rasterToPoints(test.vwc.july.sep),
        by=c('x','y'))

#remove zeros to remove inf values. 
merge.transp.storage.july.sep <-merge.transp.storage.july.sep %>%
  dplyr::filter(layer>0.01) %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0.01)

merge.transp.storage.july.sep$turnover <- 
  merge.transp.storage.july.sep$layer/merge.transp.storage.july.sep$canopy_transpiration_mm_m2

# hist(merge.transp.storage.july.sep$turnover)
# 
# head(merge.transp.storage.july.sep)
# summary(merge.transp.storage.july.sep)

# filter out extreme values
july.sep.turnover.filtered <- filter_extremes_turnover(merge.transp.storage.july.sep)

summary(july.sep.turnover.filtered)
hist(july.sep.turnover.filtered$turnover)

july.sep.turnover.filtered<-july.sep.turnover.filtered[c(1,2,5)]
july.sep.turnover.filtered.raster<-rasterFromXYZ(july.sep.turnover.filtered)
crs(july.sep.turnover.filtered.raster) <-'+proj=longlat +datum=WGS84 +no_defs'
plot(july.sep.turnover.filtered.raster,main='July-September')

#save raster
# writeRaster(july.sep.turnover.filtered.raster,
#             './../../../Data/Derived_data/Turnover/Seasonal/July_September_Turnover.tif',
#             overwrite=TRUE)

#give season ID for later merge
july.sep.turnover.filtered$season <-'july_september'



#
#

# now to october-December


#filter october-december
test.transp.oct.dec <- test.transp %>%
  dplyr::filter(month > 9) 
#unique(test.transp.oct.dec$month)

# Average across months 
test.transp.oct.dec <-aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.transp.oct.dec)
#head(test.transp.oct.dec)

#get to daily transpiration
test.transp.oct.dec$canopy_transpiration_mm_m2 <- test.transp.oct.dec$canopy_transpiration_mm_m2/90

test.transp.oct.dec <- rasterFromXYZ(test.transp.oct.dec)

test.transp.oct.dec <- resample(test.transp.oct.dec,transit.all.raster)

#save raster
# writeRaster(test.transp.oct.dec,'./../../../Data/Derived_data/ET/Seasonal/October_December_Transp.tif',
#             overwrite=TRUE)

#plot(test.transp.oct.dec)

#combine and get turnover

merge.transp.storage.oct.dec<-
  merge(rasterToPoints(test.transp.oct.dec),rasterToPoints(test.vwc.oct_dec),
        by=c('x','y'))

#remove zeros to remove inf values. 
merge.transp.storage.oct.dec <-merge.transp.storage.oct.dec %>%
  dplyr::filter(layer>0.01) %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0.01)

merge.transp.storage.oct.dec$turnover <- 
  merge.transp.storage.oct.dec$layer/merge.transp.storage.oct.dec$canopy_transpiration_mm_m2

# hist(merge.transp.storage.oct.dec$turnover)
# 
# head(merge.transp.storage.oct.dec)
# summary(merge.transp.storage.oct.dec)

# filter out extreme values
oct.dec.turnover.filtered <- filter_extremes_turnover(merge.transp.storage.oct.dec)

summary(oct.dec.turnover.filtered)
hist(oct.dec.turnover.filtered$turnover)

oct.dec.turnover.filtered<-oct.dec.turnover.filtered[c(1,2,5)]
oct.dec.turnover.filtered.raster<-rasterFromXYZ(oct.dec.turnover.filtered)
crs(oct.dec.turnover.filtered.raster) <-'+proj=longlat +datum=WGS84 +no_defs'
plot(oct.dec.turnover.filtered.raster,main='October-December')

#save raster
# writeRaster(oct.dec.turnover.filtered.raster,
#             './../../../Data/Derived_data/Turnover/Seasonal/October_December_Turnover.tif',
#             overwrite=TRUE)

#give season ID for later merge
oct.dec.turnover.filtered$season <-'october_december'

#
#

# bind all the seasonals to get more summary stats

bind.seasonal <- rbind(oct.dec.turnover.filtered,jan.march.turnover.filtered,
                       april.june.turnover.filtered,july.sep.turnover.filtered)
head(bind.seasonal)

#look at average global turnover by season
seasonal_means<-aggregate(turnover~season,mean,data=bind.seasonal)
#July -September by fair has the fastest turnover (~4.7 days)
# October-December by far has slowest turnover (~50 days)
barplot(turnover~season, data=seasonal_means)

#look at maximum seasonal turnover
seasonal_max <- aggregate(turnover~x+y,max,data=bind.seasonal)
hist(seasonal_max$turnover)
summary(seasonal_max) 

#global median maximum turnover is 18 days. The fastest seasonal turnover is
# 1.2 days all the way to roughly 1 year.

seasonal_max_raster <- rasterFromXYZ(seasonal_max)
crs(seasonal_max_raster) <- '+proj=longlat +datum=WGS84 +no_defs'

# writeRaster(seasonal_max_raster,
#             './../../../Data/Derived_data/Turnover/Seasonal/Seasonal_Maximum_Turnover.tif',
#             overwrite=TRUE)
#plot(seasonal_max_raster,main-'Maximum Seasonal Turnover')

#mask by the land cover layers
sesonal_maximum_masked <- mask(seasonal_max_raster,transit.all.raster)
plot(sesonal_maximum_masked)
summary(sesonal_maximum_masked)

#subset by specific land cover layers

#grassland
grasslandraster <-resample(grasslandraster,transit.all.raster)
grasslands_max_turnover <-mask(sesonal_maximum_masked,grasslandraster)
hist(grasslands_max_turnover$turnover)
plot(grasslands_max_turnover)
summary(grasslands_max_turnover)

#forest
forestraster <-resample(forestraster,transit.all.raster)
forest_max_turnover <-mask(sesonal_maximum_masked,forestraster)
hist(forest_max_turnover$turnover)
plot(forest_max_turnover)
summary(forest_max_turnover)

#cropland
croplandraster <-resample(croplandraster,transit.all.raster)
cropland_max_turnover <-mask(sesonal_maximum_masked,croplandraster)
hist(cropland_max_turnover$turnover)
plot(cropland_max_turnover)
summary(cropland_max_turnover)

#shrubland
shrublandraster <-resample(shrublandraster,transit.all.raster)
shrubland_max_turnover <-mask(sesonal_maximum_masked,shrublandraster)
hist(shrubland_max_turnover$turnover)
plot(shrubland_max_turnover)
summary(shrubland_max_turnover)

#tundra

tundraraster <-resample(tundraraster,transit.all.raster)
tundra_max_turnover <-mask(sesonal_maximum_masked,tundraraster)
hist(tundra_max_turnover$turnover)
plot(tundra_max_turnover)
summary(tundra_max_turnover)



