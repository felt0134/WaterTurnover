
# generate key derived datasets needed to estimate turnover

# Aggregate Global carbon density for year 2010 ----------------------------------------

#upload original
aboveground_biomass <- raster('./../../Data/Biomass/Global_Maps_C_Density_2010_1763/data/aboveground_biomass_carbon_2010.tif')
#plot(aboveground_biomass)

#aggregate 30X to match resolution of canopy trasnpiration data
aboveground_biomass_30x_aggregate <- raster::aggregate(aboveground_biomass,fact=30)
#plot(aboveground_biomass_30x_aggregate)

#save to file
writeRaster(aboveground_biomass_30x_aggregate,'./../../Data/Derived_Data/Biomass/aboveground_biomass_aggregate_30X.tif')
#-------------------------------------------------------------------------------
# Create land cover rasters and save them to the derived data folder------

# citation:
# Liu, Han, et al. "Annual dynamics of global land cover and its long-term changes from 1982 to 2015." 
# Earth System Science Data 12.2 (2020): 1217-1243.


# use most recent year (2015) in the multi-year dataset of landcover
land_cover <- raster('./../../../Data/Land_cover/GLASS-GLC/GLASS-GLC_7classes_2015.tif')
plot(land_cover)
unique(land_cover$GLASS.GLC_7classes_2015)

#just do each manually, not that many land cover types

land_cover_df<-rasterToPoints(land_cover)
land_cover_df <- data.frame(land_cover_df)
#head(land_cover_df)

#subset # 10 which is number for Cropland
ten<-subset(land_cover_df,GLASS.GLC_7classes_2015=='10')
ten.raster<-rasterFromXYZ(ten)
plot(ten.raster)
writeRaster(ten.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Cropland.tif')

#subset # 20 which is number for forest
twenty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='20')
twenty.raster<-rasterFromXYZ(twenty)
plot(twenty.raster)
writeRaster(twenty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Forest.tif')

#subset # 30 which is number for grassland
thirty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='30')
thirty.raster<-rasterFromXYZ(thirty)
plot(thirty.raster)
writeRaster(thirty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Grassland.tif')

#subset # 40 which is number for shrubland
forty<-subset(land_cover_df,GLASS.GLC_7classes_2015=='40')
forty.raster<-rasterFromXYZ(forty)
plot(forty.raster)
writeRaster(forty.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Shrubland.tif')

#subset # 70 which is number for Tundra
seventy<-subset(land_cover_df,GLASS.GLC_7classes_2015=='70')
seventy.raster<-rasterFromXYZ(seventy)
plot(seventy.raster)
writeRaster(seventy.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Tundra.tif')

#subset # 90 which is number for barren land
ninety<-subset(land_cover_df,GLASS.GLC_7classes_2015=='90')
ninety.raster<-rasterFromXYZ(ninety)
plot(ninety.raster)
writeRaster(ninety.raster,'./../../../Data/Derived_Data/Land_Cover_Distributions/Barren.tif')

#-------------------------------------------------------------------------------
# Create land cover aboveground biomass rasters --------

#import the aggregated aboveground biomass data set
aboveground_biomass <- raster('./../../../Data/Derived_data/Biomass/aboveground_biomass_aggregate_30X.tif')
#plot(aboveground_biomass)

# since not many land cover types, do this manually for each:

# land cover for grasslands

grasslands<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
#plot(grasslands)
proj4string(grasslands) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
grasslands_2<-resample(grasslands,aboveground_biomass)
grasslands.2_abg<-mask(aboveground_biomass,grasslands_2)
#plot(grasslands.2_abg)
writeRaster(grasslands.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Grasslands.tif')

# land cover for Forests

forest<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
#plot(forest)
proj4string(forest) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
forest_2<-resample(forest,aboveground_biomass)
forest.2_abg<-mask(aboveground_biomass,forest_2)
#plot(forest.2_abg)
writeRaster(forest.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif')

# land cover for Tundra

tundra<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
#plot(tundra)
proj4string(tundra) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
tundra_2<-resample(tundra,aboveground_biomass)
tundra.2_abg<-mask(aboveground_biomass,tundra_2)
#plot(tundra.2_abg)
writeRaster(tundra.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Tundra.tif')

# land cover for Shrubland

Shrubland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif')
#plot(Shrubland)
proj4string(Shrubland) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
Shrubland_2<-resample(Shrubland,aboveground_biomass)
Shrubland.2_abg<-mask(aboveground_biomass,Shrubland_2)
#plot(Shrubland.2_abg)
writeRaster(Shrubland.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Shrubland.tif')

# land cover for Cropland

Cropland<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
#plot(Cropland)
proj4string(Cropland) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
Cropland_2<-resample(Cropland,aboveground_biomass)
Cropland.2_abg<-mask(aboveground_biomass,Cropland_2)
#plot(Cropland.2_abg)
writeRaster(Cropland.2_abg,'./../../../Data/Derived_data/Biomass/Land_Cover/Cropland.tif')


#-------------------------------------------------------------------------------
# Load in and format TRY leaf water content data -----

#get TRY leaf water content data and merge it with a taxonomy data
# base that can let us know families of the species

#load original data downloaded from TRY
water.content.try<-read.delim('./../../../Data/water.content.try/14187.txt')
#head(water.content.try)

#trim down columns
water.content.try<-water.content.try %>% select(SpeciesName,DatasetID,ObservationID, DataName,OrigValueStr,OrigUnitStr)

#sort data IDs and convert to long form
water.content.try.water.content <- water.content.try %>%
  dplyr::filter(DataName=='Leaf water content per leaf dry mass')
#head(water.content.try.water.content)

#reduce and rename column names
water.content.try.water.content<-water.content.try.water.content[c(1,2,3,5,6)]
colnames(water.content.try.water.content) <-c('SpeciesName','DatasetID','ObservationID','water.content','units')
water.content.try.water.content$water.content<-as.numeric(as.character(water.content.try.water.content$water.content)) #make numeric
water.content.try.water.content$water.content<-round(water.content.try.water.content$water.content,2) #round 2 decimals
#summary(water.content.try.water.content)

#get latitude
water.content.try.lat <- water.content.try %>%
  dplyr::filter(DataName=='Latitude')
head(water.content.try.lat)

#reduce and rename column names
water.content.try.lat<-water.content.try.lat[c(1,2,3,5)]
colnames(water.content.try.lat) <-c('SpeciesName','DatasetID','ObservationID','Latitude')

#get rid of odd 212 site (maybe add later)
water.content.try.lat.no212 <- water.content.try.lat %>%
  dplyr::filter(!DatasetID =='212')
head(water.content.try.lat.no212)

# #fix up latitude to get ready for conversion to DCMs
# water.content.try.lat$Latitude <- gsub('\xb011','d',water.content.try.lat$Latitude)
# water.content.try.lat$Latitude.test<-char2dms("51d'41?N")
# water.content.try.lat$Latitude <- gsub('?','\"',water.content.try.lat$Latitude)

#get longitude
water.content.try.lon <- water.content.try %>%
  dplyr::filter(DataName=='Longitude')
#head(water.content.try.lon)

#reduce and rename column names
water.content.try.lon<-water.content.try.lon[c(1,2,3,5)]
colnames(water.content.try.lon) <-c('SpeciesName','DatasetID','ObservationID','Longitude')

#get rid of odd 212 site (maybe add later)
water.content.try.lon.no212 <- water.content.try.lon %>%
  dplyr::filter(!DatasetID =='212')
#head(water.content.try.lon.no212)

#bind the lat and lon
lat.lon.no.212<-merge(water.content.try.lon.no212,water.content.try.lat.no212,by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat.lon.no.212)

#merge with water content
lat.lon.no.212.water.content<-merge(lat.lon.no.212,water.content.try.water.content,by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat.lon.no.212.water.content)

# re-order and rename columns
col_order <- c("Longitude", "Latitude",'SpeciesName', "water.content",
               "units", "DatasetID",'ObservationID')
lat.lon.no.212.water.content<- lat.lon.no.212.water.content[, col_order]
colnames(lat.lon.no.212.water.content)<-c("x", "y",'SpeciesName', "water.content",
                                          "units", "DatasetID",'ObservationID')
#head(lat.lon.no.212.water.content)

#get family names

# species <- unique(lat.lon.no.212.water.content$SpeciesName)
# length(species)

species.list<-list()

for(i in species){
  
  test<-tax_name(i,get = c("genus","family","order"))
  species.list[[i]] <- data.frame(test)
  
}

species_df <- do.call("rbind", species.list)

colnames(species_df) <- c('db','SpeciesName','genus','family', 'order')
#head(species_df)

#merge them
species_water_content_merge<-merge(lat.lon.no.212.water.content,species_df,by=c('SpeciesName'))

#save this
write.csv(species_water_content_merge,'./../../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')

# done


#-------------------------------------------------------------------------------
# Get leaf water content data for grassland land cover (NEEDS WORK) ---------

# get leaf water content estimates for the grassland land cover

# to do:

# Will need to combine the POA filtered data frame with the dataframes for other
# families that have been double checked to see whether the family or species is herbaceous

#set projection
projection <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# derived_wc<-read.csv('./../../../Data/water.content.try/derived.dataset.csv')
derived_wc<-read.csv('./../../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')

#

#if you want to filter just to poacaea
derived_wc_poa<-subset(derived_wc,family==c('Poaceae'))
mean(derived_wc_poa$water.content)
hist(derived_wc_poa$water.content)
length(derived_wc_poa$water.content)

#if you want to get all herbs
# herb_family<-read.csv('./../../../Data/water.content.try/family.list.2.csv')
# #unique(herb_family$notes)
# # herb_family<-subset(herb_family,Herb.=='Yes')
# # family.list <- unique(herb_family$family)
# 
# #if you want to look at the 'mostly herb' species
# herb_family<-subset(herb_family,notes=='mostly_herb')
# family.list <- unique(herb_family$family)
# 
# herbs.list <- list()
# 
# for(i in family.list){
#   
#   herbs<-subset(derived_wc,family==i)
#   herbs.list[[i]] <- herbs
#   
# }

#herbs.list.df <- do.call("rbind",herbs.list)
#unique(herbs.list.df$SpeciesName)
#about 700 more observations than just poa

#create and save a list of mostly 'herb species' to X2 check which species are herb
# mostly.herb.list<-data.frame(unique(herbs.list.df$SpeciesName))
# colnames(mostly.herb.list) <- 'Species'
#write.csv(mostly.herb.list,'./../../../Data/Derived_Data/Land_Cover_Water_Content/mostly_herb_X2_check.csv')

#after checking which species are truly herbaceous in the mostly_herb_X2_check data frame,
#now you can load in the X2 checked data frame
herbx2.list <-read.csv('./../../../Data/water.content.try/mostly_herb_X2_check.csv')
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
head(herbsx2.list.df)

#take the means of each coordinate and turn into raster (do this once)
#derived_wc_mean<-aggregate(water.content~x+y,mean,data=derived_wc)

#just poa
derived_wc_mean_poa<-aggregate(water.content~x+y,mean,data=derived_wc_poa)
head(derived_wc_mean_poa)

#all herb or mostly herb (depends whats upstream of this)
derived_wc_mean_herbs<-aggregate(water.content~x+y,mean,data=herbsx2.list.df)
head(derived_wc_mean_herbs)

#bind Poa and all other herbaceous species (pixel averages)
derived_wc_mean <- rbind(derived_wc_mean_poa,derived_wc_mean_herbs)

#STOPPED HERE

#all mostly herbs

est_fix_wc_grid<-fix_grid(derived_wc_mean)
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

#just poa
#write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content.csv')

#all herb
# write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_all_herb_families.csv')

# all 'mostly herb' 
#write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_mostly_herb_families.csv')

# Poa and herb X2 checked
write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')

# #compare
# poa<-read.csv('./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content.csv')
# summary(poa)
# 
# #all herb
# all_herb<-read.csv('./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_all_herb_families.csv')
# summary(all_herb)
# 
# #mostly herb
# mostly_herb<-read.csv('./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_mostly_herb_families.csv')
# summary(mostly_herb)



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
# Import and aggregate Aboveground Biomass Density for your 2010 --------

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
# Import and re-grid climate data-----

#re-gridded aridity 
mean_aridity <- raster_from_nc_expand_grid('./../../../Data/climate/aridity.nc',
                                           'aridity20yrs')
#save
writeRaster(mean_aridity,'./../../../Data/Derived_data/Climate/mean_aridity.tif',
             overwrite=TRUE)

