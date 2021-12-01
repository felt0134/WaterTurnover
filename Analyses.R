


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#------------    CLIIMATE RELATIONSHIPS     --------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ANNUAL TRANSIT TIME ----

#aridity classification reference:
#https://wad.jrc.ec.europa.eu/sites/default/files/subchapters/9_1_Aridity/91_aridity_table.jpg

# first loop through land covers to create a global raster


#annual turnover: truncate the distributions for each land cover type
land_covers <-c('grassland','forest','tundra','cropland','shrubland')
trun.annual.list<-list()
trun.annual.list_2<-list()

for(i in land_covers){
  
  #truncate
  test.trunc <- get_turncated_dist(i,annual=T)
  
  if(i=='grassland'){
  
    test.trunc$cover <- 1
    
    }else if(i=='forest'){
    
      test.trunc$cover <- 2
      
    }else if(i=='tundra'){
      
      test.trunc$cover <- 3
      
    }else if(i=='cropland'){
      
      test.trunc$cover <- 4
      
    }else if(i=='shrubland'){
      
      test.trunc$cover <- 5
      
    }else if(i=='xxx'){xxx}
    
  
  #convert to raster: transit time
  test.trunc_raster <- rasterFromXYZ(test.trunc[c(1,2,3)])
  crs(test.trunc_raster) <- '+proj=longlat +datum=WGS84'
  
  trun.annual.list[[i]] <- test.trunc_raster
  
  #convert to raster: land cover ID
  test.trunc_raster_2 <- rasterFromXYZ(test.trunc[c(1,2,4)])
  crs(test.trunc_raster_2) <- '+proj=longlat +datum=WGS84'
  trun.annual.list_2[[i]] <- test.trunc_raster_2
  
  }
  


#bind all rasters #1
global_truncated<-
  raster::merge(trun.annual.list[1]$grassland,trun.annual.list[2]$forest,
                trun.annual.list[3]$tundra,trun.annual.list[4]$cropland,
                trun.annual.list[5]$shrubland)
#plot(global_truncated)

#bind all rasters #2
global_truncated_2<-
  raster::merge(trun.annual.list_2[1]$grassland,trun.annual.list_2[2]$forest,
                trun.annual.list_2[3]$tundra,trun.annual.list_2[4]$cropland,
                trun.annual.list_2[5]$shrubland)
#plot(global_truncated_2)

#end


#
#




#loop through each climate covar to resample to the annualimum transit raster...
#start:

climate_vars <-c('aridity','pet','precip')
climate_list_annual <- list()

for(i in climate_vars){
  
  aridity <- raster(paste0('./../../../Data/Derived_data/Climate/mean_',i,'.tif'))
  
  aridity_annual<-resample(aridity,global_truncated)
  plot(aridity_annual)
  
  #convert to dataframes and and merge
  
  aridity_annual_df <- data.frame(rasterToPoints(aridity_annual))
  head(aridity_annual_df)
  
  annual_turnover_df <- data.frame(rasterToPoints(global_truncated))
  head(annual_turnover_df)
  
  annual_turnover_aridity <- merge(annual_turnover_df,aridity_annual_df,by=c('x','y'))
  head(annual_turnover_aridity)
  
  annual_turnover_df_2 <- data.frame(rasterToPoints(global_truncated_2))
  annual_turnover_df_2$cover <- annual_turnover_df_2$layer
  head(annual_turnover_df_2)
  
  annual_turnover_aridity <- merge(annual_turnover_df_2[c(1,2,4)],annual_turnover_aridity,by=c('x','y'))
  head(annual_turnover_aridity)
  
  annual_turnover_aridity$cover<-as.factor(annual_turnover_aridity$cover)
  
  annual_turnover_aridity$cover <- recode_factor(annual_turnover_aridity $cover, 
                                                 '1' = "grassland", 
                                                 '2' = "forest",
                                                 '3'='tundra',
                                                 '4'='cropland',
                                                 '5'='shrubland')
  
  #head(annual_turnover_annual_aridity)
  
  climate_list_annual[[i]] <- annual_turnover_aridity
  
}



#end

#run spatial models and save coefficients to file

library(spatialEco)

# Annual turnover models
climate_list <- climate_list_annual
turnover_scale <- 'annual'

#aridity
climate_name = 'aridity'
climate_variable = 1
source('Spatial_Model_Script.R')

#pet
climate_name = 'pet'
climate_variable = 2
source('Spatial_Model_Script.R')

#MAP
climate_name = 'ppt'
climate_variable = 3
source('Spatial_Model_Script.R')


#
#


#Notes
#prelim analyses of each covariate showed: 

# Aridity: much more variation explained when separating by land cover type. Log
# transformation needed to most closely meet assumptions of LM

# PET: Explains a lot more variation by itself than aridity. However, more
# variation is still explained when separating by land cover type. Log transformation
# needed to most closely meet assumptions of LM

# MAP: Same outcome as aridity.


#-------------------------------------------------------------------------------
#MINIMUM TRANSIT TIME------

# first loop through land covers to create a global raster

#start:
trun.minimum.list<-list()
trun.minimum.list_2<-list()
land_covers_2 <- c("grassland","forest","tundra","cropland","shrubland")

for(i in land_covers_2){
  
  #truncate
  test.trunc <- get_turncated_dist(i,annual=F)
  
  if(i=='grassland'){
    
    test.trunc$cover <- 1
    
  }else if(i=='forest'){
    
    test.trunc$cover <- 2
    
  }else if(i=='tundra'){
    
    test.trunc$cover <- 3
    
  }else if(i=='cropland'){
    
    test.trunc$cover <- 4
    
  }else if(i=='shrubland'){
    
    test.trunc$cover <- 5
    
  }else if(i=='xxx'){xxx}
  
  test.trunc_min_raster <- rasterFromXYZ(test.trunc[c(1,2,3)])
  crs(test.trunc_min_raster) <- '+proj=longlat +datum=WGS84'
  
  trun.minimum.list[[i]] <- test.trunc_min_raster
  
  test.trunc_min_raster_2 <- rasterFromXYZ(test.trunc[c(1,2,4)])
  crs(test.trunc_min_raster_2) <- '+proj=longlat +datum=WGS84'
  
  trun.minimum.list_2[[i]] <- test.trunc_min_raster_2
  
}

#bind rasters #1
global_truncated_minimum <-
  raster::merge(trun.minimum.list[1]$grassland,trun.minimum.list[2]$forest,
                trun.minimum.list[3]$tundra,trun.minimum.list[4]$cropland,
                trun.minimum.list[5]$shrubland)
plot(global_truncated_minimum)

#bind rasters #2
global_truncated_minimum_2 <-
  raster::merge(trun.minimum.list_2[1]$grassland,trun.minimum.list_2[2]$forest,
                trun.minimum.list_2[3]$tundra,trun.minimum.list_2[4]$cropland,
                trun.minimum.list_2[5]$shrubland)

#plot(global_truncated_minimum_2)

#end


#
#


#loop through each climate covar to resample to the minimum transit raster...
#start:

climate_vars <-c('aridity','pet','precip')
climate_list_minimum <- list()

for(i in climate_vars){

aridity <- raster(paste0('./../../../Data/Derived_data/Climate/mean_',i,'.tif'))

aridity_min<-resample(aridity,global_truncated_minimum)
plot(aridity_min)

#convert to dataframes and and merge

aridity_min_df <- data.frame(rasterToPoints(aridity_min))
head(aridity_min_df)

annual_turnover_min_df <- data.frame(rasterToPoints(global_truncated_minimum))
head(annual_turnover_min_df)

annual_turnover_min_aridity <- merge(annual_turnover_min_df,aridity_min_df,by=c('x','y'))
head(annual_turnover_min_aridity)

annual_turnover_min_df_2 <- data.frame(rasterToPoints(global_truncated_minimum_2))
annual_turnover_min_df_2$cover <- annual_turnover_min_df_2$layer
head(annual_turnover_min_df_2)

annual_turnover_min_aridity <- merge(annual_turnover_min_df_2[c(1,2,4)],annual_turnover_min_aridity,by=c('x','y'))
head(annual_turnover_min_aridity)

annual_turnover_min_aridity$cover<-as.factor(annual_turnover_min_aridity$cover)

annual_turnover_min_aridity$cover <- recode_factor(annual_turnover_min_aridity $cover, 
                                               '1' = "grassland", 
                                               '2' = "forest",
                                               '3'='tundra',
                                               '4'='cropland',
                                               '5'='shrubland')

#head(annual_turnover_min_aridity)

climate_list_minimum[[i]] <- annual_turnover_min_aridity

}

#end

#run models

# Minimum turnover models
climate_list <- climate_list_minimum
turnover_scale <- 'minimum'

#aridity
climate_name = 'aridity'
climate_variable = 1
source('Spatial_Model_Script.R')

#pet
climate_name = 'pet'
climate_variable = 2
source('Spatial_Model_Script.R')

#MAP
climate_name = 'ppt'
climate_variable = 3
source('Spatial_Model_Script.R')




#-------------------------------------------------------------------------------
#compare correlations among/between aridity, PET, and MAP (Needs work) -----

# assumes that first part of annual transit time section was run

#load data
aridity <- raster('./../../../Data/Derived_data/Climate/mean_aridity.tif')
mean_ppt <- raster('./../../../Data/Derived_data/Climate/mean_precip.tif')
mean_pet <- raster('./../../../Data/Derived_data/Climate/mean_pet.tif')
plot(mean_pet)
#resample climate data

#resample
aridity<-resample(aridity,global_truncated)
mean_ppt<-resample(mean_ppt,global_truncated)
mean_pet<-resample(mean_pet,global_truncated)

#data frame and merge aridity
aridity_turnover_df <- merge(data.frame(rasterToPoints(aridity)),
                             data.frame(rasterToPoints(global_truncated)),
                             by=c('x','y'))
#head(aridity_turnover_df)
cor(aridity_turnover_df$mean_aridity,aridity_turnover_df$layer)
#0.022

#data frame and merge precip
precip_turnover_df <- merge(data.frame(rasterToPoints(mean_ppt)),
                             data.frame(rasterToPoints(global_truncated)),
                             by=c('x','y'))

#head(precip_turnover_df)
cor(precip_turnover_df$mean_precip,precip_turnover_df$layer)
#-0.28

#data frame and merge PET
pet_turnover_df <- merge(data.frame(rasterToPoints(mean_pet)),
                            data.frame(rasterToPoints(global_truncated)),
                            by=c('x','y'))
#head(pet_turnover_df)
cor(pet_turnover_df$mean_pet,pet_turnover_df$layer)
#-0.47

#look at these relationships a bit closer
library(splitstackshape)

#PET
test.strat.pet<-stratified(pet_turnover_df, c("y"), 0.001)
plot(layer~mean_pet,data=test.strat.pet)
summary(lm(layer~mean_pet,data=pet_turnover_df))
# as PET increases, turnover time decreases. Explains ~22% of variation

#PPT
test.strat.ppt<-stratified(precip_turnover_df, c("y"), 0.001)
plot(layer~mean_precip,data=test.strat.ppt)
summary(lm(layer~mean_precip,data=precip_turnover_df))
# at PPT increases, turnover time decreases. Explains ~8% of variation






#-------------------------------------------------------------------------------


