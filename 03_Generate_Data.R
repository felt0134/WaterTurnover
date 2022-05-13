
# generate key derived datasets needed to estimate turnover

#-------------------------------------------------------------------------------
# Create land cover rasters and save them to the derived data folder------

# citation:
# Liu, Han, et al. "Annual dynamics of global land cover and its long-term changes from 1982 to 2015." 
# Earth System Science Data 12.2 (2020): 1217-1243.


# use most recent year (2015) in the multi-year dataset of landcover
land_cover <- raster('./../../../Data/Land_cover/GLASS-GLC/GLASS-GLC_7classes_2015.tif')
# plot(land_cover)
# unique(land_cover$GLASS.GLC_7classes_2015)
#0  10  20  30  40  70  90 100

#just do each manually, not that many land cover types

land_cover_df<-rasterToPoints(land_cover)
land_cover_df <- data.frame(land_cover_df)
#head(land_cover_df)




#subset # 10 which is number for Cropland
cropland_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Cropland.tif')
ten<-subset(land_cover_df,GLASS.GLC_7classes_2015=='10')
ten.raster<-rasterFromXYZ(ten)
proj4string(ten.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
ten.raster <- resample(ten.raster,cropland_biomass)
plot(ten.raster)
writeRaster(ten.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Cropland.tif',overwrite=TRUE)
rm(ten,ten.raster,cropland_biomass)

#subset # 20 which is number for forest
forest_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif')
twenty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='20')
twenty.raster<-rasterFromXYZ(twenty)
proj4string(twenty.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
twenty.raster <- resample(twenty.raster,forest_biomass) #resample to biomass extent and resolution
plot(twenty.raster)
writeRaster(twenty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Forest.tif',overwrite=TRUE)
rm(twenty,twenty.raster,forest_biomass)

#subset # 30 which is number for grassland
grassland_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Grassland.tif')
thirty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='30')
thirty.raster<-rasterFromXYZ(thirty)
proj4string(thirty.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
thirty.raster <- resample(thirty.raster,grassland_biomass) #resample to biomass extent and resolution
plot(thirty.raster)
writeRaster(thirty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Grassland.tif',overwrite=TRUE)
rm(thirty,thirty.raster,grassland_biomass)

#subset # 40 which is number for shrubland
shrubland_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Shrubland.tif')
forty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='40')
forty.raster<-rasterFromXYZ(forty)
proj4string(forty.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
forty.raster <- resample(forty.raster,shrubland_biomass) #resample to biomass extent and resolution
plot(forty.raster)
writeRaster(forty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Shrubland.tif',overwrite=TRUE)
rm(forty,forty.raster,shrubland_biomass)

#subset # 70 which is number for Tundra
tundra_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Tundra.tif')
seventy<-subset(land_cover_df,GLASS.GLC_7classes_2015=='70')
seventy.raster<-rasterFromXYZ(seventy)
proj4string(seventy.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
seventy.raster <- resample(seventy.raster,tundra_biomass) #resample to biomass extent and resolution
plot(seventy.raster)
writeRaster(seventy.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Tundra.tif',overwrite=TRUE)
rm(seventy,seventy.raster,tundra_biomass)

#subset # 90 which is number for barren land
barren_biomass<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Barren.tif')
ninety<-subset(land_cover_df,GLASS.GLC_7classes_2015=='90')
ninety.raster<-rasterFromXYZ(ninety)
proj4string(ninety.raster) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
ninety.raster <- resample(ninety.raster,barren_biomass) #resample to biomass extent and resolution
plot(ninety.raster)
writeRaster(ninety.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Barren.tif',overwrite=TRUE)
rm(ninety,ninety.raster)

#-------------------------------------------------------------------------------
# Create land cover spatial polygons -----

#grassland
shapefile.grassland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
shapefile.grassland <- aggregate(shapefile.grassland,fact=10)
plot(shapefile.grassland,col='blue')
shapefile.grassland<-rasterToPolygons(shapefile.grassland)
writeOGR(shapefile.grassland,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/grassland',
         driver="ESRI Shapefile", layer='grassland',overwrite_layer = T)
rm(shapefile.grassland)

#forest
shapefile.forest<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
shapefile.forest<-rasterToPolygons(shapefile.forest)
writeOGR(shapefile.forest,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/forest',
         driver="ESRI Shapefile", layer='forest')
rm(shapefile.forest)

#shrubland
shapefile.shrubland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
shapefile.shrubland<-rasterToPolygons(shapefile.shrubland)
writeOGR(shapefile.shrubland,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/shrubland',
         driver="ESRI Shapefile", layer='shrubland')
rm(shapefile.shrubland)

#cropland
shapefile.shrubland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
shapefile.shrubland<-rasterToPolygons(shapefile.shrubland)
writeOGR(shapefile.shrubland,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/shrubland',
         driver="ESRI Shapefile", layer='shrubland')
rm(shapefile.shrubland)

#cropland
shapefile.cropland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
shapefile.cropland<-rasterToPolygons(shapefile.cropland)
writeOGR(shapefile.cropland,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/cropland',
         driver="ESRI Shapefile", layer='cropland')
rm(shapefile.cropland)

#tundra
shapefile.tundra<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
shapefile.tundra<-rasterToPolygons(shapefile.tundra)
writeOGR(shapefile.tundra,'./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/tundra',
         driver="ESRI Shapefile", layer='tundra')
rm(shapefile.tundra)

#-------------------------------------------------------------------------------
# Create land cover aboveground biomass rasters --------

#import the aggregated aboveground biomass data set
aboveground_biomass <- raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')
#plot(aboveground_biomass)

# since not many land cover types, do this manually for each:

# land cover for grasslands
grasslands<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
#plot(grasslands)
proj4string(grasslands) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
grasslands_2<-resample(grasslands,aboveground_biomass)
grasslands.2_abg<-mask(aboveground_biomass,grasslands_2)
#plot(grasslands.2_abg)
writeRaster(grasslands.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Grassland.tif',overwrite=TRUE)
rm(grasslands,grasslands_2,grasslands.2_abg)

# land cover for Forests
forest<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
#plot(forest)
proj4string(forest) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
forest_2<-resample(forest,aboveground_biomass)
forest.2_abg<-mask(aboveground_biomass,forest_2)
#plot(forest.2_abg)
writeRaster(forest.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif',overwrite=TRUE)
rm(forest.2_abg,forest_2,forest)

# land cover for Tundra
tundra<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
#plot(tundra)
proj4string(tundra) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
tundra_2<-resample(tundra,aboveground_biomass)
tundra.2_abg<-mask(aboveground_biomass,tundra_2)
#plot(tundra.2_abg)
writeRaster(tundra.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Tundra.tif',overwrite=TRUE)
rm(tundra,tundra.2_abg,tundra_2)

# land cover for Shrubland
Shrubland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
#plot(Shrubland)
proj4string(Shrubland) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
Shrubland_2<-resample(Shrubland,aboveground_biomass)
Shrubland.2_abg<-mask(aboveground_biomass,Shrubland_2)
#plot(Shrubland.2_abg)
writeRaster(Shrubland.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Shrubland.tif',overwrite=TRUE)
rm(Shrubland,Shrubland.2_abg,Shrubland_2)

# land cover for Cropland
Cropland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
#plot(Cropland)
proj4string(Cropland) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
Cropland_2<-resample(Cropland,aboveground_biomass)
Cropland.2_abg<-mask(aboveground_biomass,Cropland_2)
#plot(Cropland.2_abg)
writeRaster(Cropland.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Cropland.tif',overwrite=TRUE)
rm(Cropland,Cropland.2_abg,Cropland_2)

# land cover for barren
barren<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/barren.tif')
#plot(barren)
proj4string(barren) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
barren_2<-resample(barren,aboveground_biomass)
barren.2_abg<-mask(aboveground_biomass,barren_2)
#plot(barren.2_abg)
writeRaster(barren.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Barren.tif',overwrite=TRUE)
rm(barren,barren.2_abg,barren_2)

rm(aboveground_biomass)


#-------------------------------------------------------------------------------
# Load in and format TRY leaf water content data -----

#get TRY leaf water content data and merge it with a taxonomy data
# base that can let us know families of the species

#load original data downloaded from TRY
water_content_try<-read.delim('./../../Data/water.content.try/14187.txt')
#head(water.content.try)

#trim down columns
water_content_try <- water_content_try %>% 
  select(SpeciesName,DatasetID,ObservationID, DataName,OrigValueStr,OrigUnitStr)

#sort data IDs and convert to long form

#isolate ldmc
water_content_try_id <- water_content_try %>%
  dplyr::filter(DataName=='Leaf water content per leaf dry mass')
#head(water_content_try_id)

#reduce and rename column names
water_content_try_id <- water_content_try_id %>% 
  select(SpeciesName,DatasetID,ObservationID,DataName,OrigValueStr,OrigUnitStr) %>%
  rename('water_content' = 'OrigValueStr',
         'units' = 'OrigUnitStr')

#change water content to numeric and round to two decimal places
water_content_try_id$water_content <- 
  as.numeric(as.character(water_content_try_id$water_content)) #make numeric

water_content_try_id$water_content <- 
  round(water_content_try_id$water_content,2) #round 2 decimals
#summary(water_content_try_id)

#isolate latitude
water_content_try_lat <-  water_content_try %>%
  dplyr::filter(DataName=='Latitude') %>%
  select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  rename('Latitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212

#isolate longitude
water_content_try_lon <-  water_content_try %>%
  dplyr::filter(DataName=='Longitude') %>%
  select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  rename('Longitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212

#bind the lat and lon
lat_lon_try <- merge(water_content_try_lat,water_content_try_lon,
        by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat_lon_try,1)

#merge with water content
lat_lon_try_water_content <- merge(lat_lon_try,water_content_try_id,
                                    by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat_lon_try_water_content,1)

#reorder and rename columns
lat_lon_try_water_content <- lat_lon_try_water_content %>%
  select("Longitude", "Latitude",'SpeciesName', "water_content",
         "units", "DatasetID",'ObservationID') %>%
  rename("x" = "Longitude",
         "y" = "Latitude")


#head(lat_lon_try_water_content,1)

#get family names

species <- unique(lat_lon_try_water_content$SpeciesName)
# length(species)

library(taxize)

species_list<-list()
for(i in species){
  
  try_taxonomy <- tax_name(i,get = c("genus","family","order"))
  species_list[[i]] <- data.frame(try_taxonomy)
  
}

species_df <- do.call("rbind", species_list)

colnames(species_df) <- c('db','SpeciesName','genus','family', 'order')
#head(species_df)

#merge them
species_water_content_merge<-merge(species_df,lat_lon_try_water_content,by=c('SpeciesName'))

#save this
write.csv(species_water_content_merge,'./../../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')


#-------------------------------------------------------------------------------
# Get leaf water content data for grassland land cover (NEEDS WORK) ---------

# get leaf water content estimates for the grassland land cover

#set projection
projection <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

#step 1: create a dataframe with list of herbaceous families to be double checked

derived_wc <- read.csv('./../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')

# get name of all herbs (x2 check where this came from)
herb_family<-read.csv('./../../Data/water.content.try/family.list.2.csv')
herb_family<-subset(herb_family,Herb.=='Yes')
family.list <- unique(herb_family$family)
# 

herbs.list <- list()

for(i in family.list){

  herbs<-subset(derived_wc,family==i)
  herbs.list[[i]] <- herbs

}

herbs.list.df <- do.call("rbind",herbs.list)
length(unique(herbs.list.df$SpeciesName))
#about 300 more observations than just poa

#create and save a list of mostly 'herb species' to X2 check which species are herb
mostly.herb.list<-data.frame(unique(herbs.list.df$SpeciesName))
colnames(mostly.herb.list) <- 'Species'
#write.csv(mostly.herb.list,'./../../../Data/Derived_Data/Land_Cover_Water_Content/mostly_herb_X2_check.csv')

#after checking which species are truly herbaceous in the mostly_herb_X2_check data frame,
#now you can load in the X2 checked data frame
herbx2.list <-read.csv('./../../Data/water.content.try/mostly_herb_X2_check.csv')
herbx2.list <- subset(herbx2.list,Herbaceous.=='Yes')
herbx2.list <- herbx2.list[c(2)]

#now loop through to get X2 checked mostly herb species
herbx2.list <- unique(herbx2.list$Species)

herbsx2.list <- list()

for(i in herbx2.list){
  
  herbs<-subset(derived_wc,SpeciesName==i)
  herbsx2.list[[i]] <- herbs
  
}

herbsx2.list.df <- do.call("rbind",herbsx2.list)
head(herbsx2.list.df,1)

#take the means of each coordinate and turn into raster (do this once)
#derived_wc_mean<-aggregate(water.content~x+y,mean,data=derived_wc)

#just poa
derived_wc_poa <- derived_wc %>% filter(family=='Poaceae')
derived_wc_mean_poa<-aggregate(water.content~x+y,mean,data=derived_wc_poa)
head(derived_wc_mean_poa,1)


#all herb 
derived_wc_mean_herbs<-aggregate(water.content~x+y,mean,data=herbsx2.list.df)
head(derived_wc_mean_herbs,1)

#bind Poa and all other herbaceous species (pixel averages) to have all herb species
derived_wc_mean <- rbind(derived_wc_mean_poa,derived_wc_mean_herbs)

#final averag
derived_wc_mean_2 <- aggregate(water.content~x+y,mean,data=derived_wc_mean)
head(derived_wc_mean_2,1)
plot(y~x,data=derived_wc_mean_2)


#new approach using nearest spatial points

#turn ground data to spdf
coords_ground <- derived_wc_mean_2[ , c("x", "y")]   # coordinates
data_ground   <- data.frame(derived_wc_mean_2[c(3)])          # data
crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

spdf_ground <- SpatialPointsDataFrame(coords      = coords_ground,
                                      data        = data_ground, 
                                      proj4string = crs)

#turn vod raster data to spatial points DF
annual_strorage_2 <- annual_turnover_lc %>%
  select(lon,lat,annual_storage,group,group_2)
rownames(annual_strorage_2) <- NULL

# prepare coordinates, data, and proj4string
coords_vod <- annual_strorage_2[ , c("lon", "lat")]   # coordinates
data_vod   <- annual_strorage_2[ , 3:5]          # data

# make the SpatialPointsDataFrame object
spdf_vod <- SpatialPointsDataFrame(coords      = coords_vod,
                                   data        = data_vod, 
                                   proj4string = crs)

library(FNN)

#link ground-based coordinates to vod coordinates for storage
nn1 = get.knnx(coordinates(spdf_vod), coordinates(spdf_ground), 1)
vector <- data.frame(nn1[1])
vector <- vector[c(1:nrow(vector)),]
spdf_vod_df <- data.frame(spdf_vod)
new_df <- spdf_vod_df[c(vector),]
new_df <- new_df %>%
  select(annual_storage,group,lon,lat)

points(lat~lon,data=new_df,add=T,col='red')

cbind_ground_vod_herb <- cbind(new_df,derived_wc_mean_2)
cbind_ground_vod_herb <- cbind_ground_vod_herb %>%
  filter(group==c('Cropland','Grassland'))

#create file for double checking
write.csv(cbind_ground_vod_herb,
          './../../Data/water.content.try/crop_grassland_mean_water_content.csv')


#olf approach to filter by making land cover a raster



#STOPPED HERE

#all mostly herbs

est_fix_wc_grid<-fix_grid(derived_wc_mean)
plot(est_fix_wc_grid)
proj4string(est_fix_wc_grid) <- CRS(projection)

grasslands<-raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Grassland.tif')

#get into the same projection
proj4string(grasslands) <- CRS(projection)

#resample so can align
resample_test<-resample(grasslands,est_fix_wc_grid)

plot(resample_test)
plot(est_fix_wc_grid)

#turn into data frame and merge
biome_raster_df<-data.frame(rasterToPoints(resample_test))
est_fix_wc_grid_df<-data.frame(rasterToPoints(est_fix_wc_grid))

test_merge<-merge(biome_raster_df,est_fix_wc_grid_df,by=c('x','y'))
test_merge <-test_merge[c(1,2,4)]
colnames(test_merge) <- c('x','y','average.water.content')
test_merge$average.water.content <- round(test_merge$average.water.content,2)
hist(test_merge$average.water.content)
summary(test_merge)

#save as csv

# Poa and herb X2 checked
write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')

#-------------------------------------------------------------------------------
# Import VOD (converted to VWC) monthly data for 2016 ------


library(rhdf5)
library(R.matlab)

# We are doing December 1 2015 - November 31 2016

#data repo
#http://afeldman.mit.edu.libproxy.chapman.edu/mt-dca-data

# get month and days
# h5read('./../../../Data/VWC/MTDCA_V4_TAU_201601_201603_9km.mat',
#        "DateVector", read.attributes = TRUE)

#January: 1-31
#February: 32-60
#March 61-91


# January to March

#January
jan<-get_vwc(x=1,y=31,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201601_201603_9km.mat')
write.csv(jan,'./../../../Data/Derived_data/VWC/vwc_2016_01.csv')
rm(jan)


#Feb
feb<-get_vwc(x=32,y=60,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201601_201603_9km.mat')
write.csv(feb,'./../../../Data/Derived_data/VWC/vwc_2016_02.csv')
rm(feb)

#March
march<-get_vwc(x=61,y=91,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201601_201603_9km.mat')
write.csv(march,'./../../../Data/Derived_data/VWC/vwc_2016_03.csv')
rm(march)

##

# April to June

# get month and days
# h5read('./../../../Data/VWC/MTDCA_V4_TAU_201604_201606_9km.mat',
#        "DateVector", read.attributes = TRUE)

#April: 1-30
#May: 31-61
#June: 62-91

#April
april<-get_vwc(x=1,y=30,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201604_201606_9km.mat')
write.csv(april,'./../../../Data/Derived_data/VWC/vwc_2016_04.csv')
rm(april)

#May
may<-get_vwc(x=31,y=61,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201604_201606_9km.mat')
write.csv(may,'./../../../Data/Derived_data/VWC/vwc_2016_05.csv')
rm(may)

#June
june<-get_vwc(x=62,y=91,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201604_201606_9km.mat')
write.csv(june,'./../../../Data/Derived_data/VWC/vwc_2016_06.csv')
rm(june)

##
# june_2 <- fix_grid(june)

# July to September

# get month and days
# h5read('./../../../Data/VWC/MTDCA_V4_TAU_201607_201609_9km.mat',
#        "DateVector", read.attributes = TRUE)

#July: 1-31
#August: 32-62
#September: 63-92

#July
july<-get_vwc(x=1,y=31,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201607_201609_9km.mat')
write.csv(july,'./../../../Data/Derived_data/VWC/vwc_2016_07.csv')
rm(july)

#August
august<-get_vwc(x=32,y=62,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201607_201609_9km.mat')
write.csv(august,'./../../../Data/Derived_data/VWC/vwc_2016_08.csv')
rm(august)

#September
september<-get_vwc(x=63,y=92,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201607_201609_9km.mat')
write.csv(september,'./../../../Data/Derived_data/VWC/vwc_2016_09.csv')
rm(september)

##

# October December

# get month and days
# h5read('./../../../Data/VWC/MTDCA_V4_TAU_201610_201612_9km.mat',
#        "DateVector", read.attributes = TRUE)

#October: 1-31
#November: 32-61
#December: 62-92

#October
october<-get_vwc(x=1,y=31,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201610_201612_9km.mat')
write.csv(october,'./../../../Data/Derived_data/VWC/vwc_2016_10.csv')
rm(october)

#November
november<-get_vwc(x=32,y=61,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201610_201612_9km.mat')
write.csv(november,'./../../../Data/Derived_data/VWC/vwc_2016_11.csv')
rm(november)

#December
december<-get_vwc(x=62,y=92,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201610_201612_9km.mat')
write.csv(december,'./../../../Data/Derived_data/VWC/vwc_2016_12.csv')
rm(december)

#we also want December 2015:

# get month and days
h5read('./../../../Data/VWC/MTDCA_V4_TAU_201510_201512_9km.mat',
       "DateVector", read.attributes = TRUE)

#December
december_2015<-get_vwc(x=62,y=92,filepath = './../../../Data/VWC/MTDCA_V4_TAU_201510_201512_9km.mat')
write.csv(december_2015,'./../../../Data/Derived_data/VWC/vwc_2015_12.csv')
rm(december_2015)





#-------------------------------------------------------------------------------
# Import and aggregate Aboveground Biomass Density for year 2010 --------

#file source:
#https://uwmadison.app.box.com/s/xj3fnde17yazlogbiq740da2mrv2ma61

#upload original
aboveground_biomass_density <- raster('./../../../Data/Biomass/Biomass_Density_Spawn/AFELTON_agbDW_Mgha_x10_300m.tif')
#plot(aboveground_biomass_density)

#aggregate 30X to match resolution of canopy trasnpiration data
aboveground_biomass_density_30x_aggregate <- raster::aggregate(aboveground_biomass_density,fact=30)
#plot(aaboveground_biomass_density_30x_aggregate)

#save to file
writeRaster(aboveground_biomass_density_30x_aggregate,'./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')
#-------------------------------------------------------------------------------
# Get leaf water content data for tundra land cover ------

tundra_data<-read.csv('./../../../Data/tundra_trait_team/TTT_cleaned_dataset.csv')
head(tundra_data)
unique(tundra_data$Trait)
tundra_ldmc <- subset(tundra_data,
                      Trait="Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)")
head(tundra_ldmc)
hist(tundra_ldmc$Value)

tundra_ldmc_averaged <- aggregate(Value~Latitude + Longitude,mean,data=tundra_ldmc)
summary(tundra_ldmc_averaged)


#unclear to what is mean by fresh mass. How you can have more dry mass than 
#fresh mass (dry+water) or is fresh mass just water?

#assume LDMC = 1.2 (1.2 g dry mass/1g fresh mass)
#So it is either:
#1/1.2 = water divided by dry mass = 0.83 (assuming fresh mass=everything)
#((1.2+1) -1.2)/(1.2+1) = 0.45 (assuming fresh mass = water)
#((1.2+1) -1)/(1.2+1) = 0.55 (must be this one?)
#-------------------------------------------------------------------------------
# Convert land cover rasters into shapefiles (work in progress) -----

forest<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
forest_ag <- raster::aggregate(forest,fact=30)
crs(forest) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grasslands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif')
#r <- forest> -Inf

#ploygons
plot(forest_ag)
forest_shp<-rasterToPolygons(forest_ag, dissolve=F)
?rasterToPolygons
plot(forest_shp)

#lines
forest_shp_lines <- rasterToContour(grasslands_error)
plot(forest_shp_lines)

crs(forest_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(forest_shp, dsn=getwd(),layer="forest_shp",driver="ESRI Shapefile")


#-------------------------------------------------------------------------------
# Import  climate data-----

#first look at ncdf file to get variable names
#climate_data <- nc_open('./../../../Data/climate/aridity.nc')

#re-gridded aridity 
mean_aridity <- raster_from_nc_expand_grid('./../../../Data/climate/aridity.nc',
                                           'aridity20yrs')
#plot(mean_aridity)

#save
# writeRaster(mean_aridity,'./../../../Data/Derived_data/Climate/mean_aridity.tif',
#              overwrite=TRUE)

#
#


#re-gridded MAP

#look at data
#map <- raster(ncvar_get(climate_data,'p20yrs'))

mean_precip <- raster_from_nc_expand_grid('./../../../Data/climate/aridity.nc',
                                           'p20yrs')

#this is daily precip, so multipy by 365 (Purdy, A., personal Corr.)
mean_precip <- mean_precip*365.25

#resample to match aridity
mean_precip <- resample(mean_precip,mean_aridity)
mean_precip <- mask(mean_precip,mean_aridity)
#plot(mean_precip)

#save
writeRaster(mean_precip,'./../../../Data/Derived_data/Climate/mean_precip.tif',
            overwrite=TRUE)




#
#


#re-gridded PET
mean_pet <- raster_from_nc_expand_grid('./../../../Data/climate/aridity.nc',
                                           'pet20yrs')
#plot(mean_pet)


#resample to match aridity
mean_pet <- resample(mean_pet,mean_aridity)
mean_pet <- mask(mean_pet,mean_aridity)
#plot(mean_pet)

#save
writeRaster(mean_pet,'./../../../Data/Derived_data/Climate/mean_pet.tif',
            overwrite=TRUE)


#temperature
#first look at ncdf file to get variable names
nc_open('./../../../Data/climate/Tmean_mean_2015_2017.nc')
# 'Tmean_mean'
plot(rasterize(climate_data_2))

temp <- raster_from_nc_expand_grid('./../../../Data/climate/Tmean_mean_2015_2017.nc',
                                   'Tmean_mean')
#plot(temp,main='mean daily temp')
head(temp)

#convert from K to C
temp_2 <- temp - 273.15
plot(temp_2)

#save
writeRaster(temp_2,'./../../../Data/Derived_data/Climate/mean_temp_2015_2017.tif',
            overwrite=TRUE)


#maximum temp
nc_open('./../../../Data/climate/Tmax_mean_2015_2017.nc')
max_temp <- raster_from_nc_expand_grid('./../../../Data/climate/Tmax_mean_2015_2017.nc',
                                   'Tmax_mean')

plot(max_temp)


#load file
nc_data <- nc_open('./../../../Data/climate/Tmax_mean_2015_2017.nc')

# Longitude 
lon <- ncvar_get(nc_data ,"lon")
dim(lon)
# Latitude 
lat <- ncvar_get(nc_data ,"lat")
dim(lat)
# Variable
var <- ncvar_get(nc_data,'Tmax_mean')

#convert to raster
latlong = expand.grid(long=lon, lat=lat)
latlong = data.frame(cbind(latlong, aridity = c(var)))
latlong = na.exclude(latlong)
summary(latlong)
colnames(latlong) <- c('x','y',variable)

#-------------------------------------------------------------------------------