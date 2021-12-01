


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# generate and save monthly transpiration raster for each land cover
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

#grassland 
ecoregion_id = 'grassland'
source('0X_generate_monthly_LC_Rasters.R')

#forest
ecoregion_id = 'forest'
source('0X_generate_monthly_LC_Rasters.R')

#shrubland
ecoregion_id = 'shrubland'
source('0X_generate_monthly_LC_Rasters.R')

#tundra
ecoregion_id = 'tundra'
source('0X_generate_monthly_LC_Rasters.R')

#cropland 
ecoregion_id = 'cropland'
source('0X_generate_monthly_LC_Rasters.R')

#barren
ecoregion_id = 'barren'
source('0X_generate_monthly_LC_Rasters.R')


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# get turnover from VWC
# this script generates land cover-specific estimates of:
# annual-scale transit time/turnover
# annual-scale vegetation water storage
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Grassland ------

#annual turnover/storage
grassland_annual_stack = import_cumulative_transp('Grassland')
ecoregion = 'grassland'
reference_raster = grassland_annual_stack
source('0X_annual_turnover_script.R')
rm(grassland_annual_stack)

#seasonal turnover/storage
grassland_seasonal <- import_monthly_transp_2('Grassland')
ecoregion = 'grassland'
transp.stack = grassland_seasonal
source('test_loop.R')
rm(grassland_seasonal,transp.stack)

#minimum turnover and pixel sample size (# months of data)
grassland_minimum <- import_monthly_transp_2('Grassland')
ecoregion = 'grassland'
transp.stack = grassland_minimum
source('0X_minimum_transit_script.R')
rm(grassland_minimum,transp.stack)

#summary(grassland_seasonal$...........Data.Derived_Data.Land_Cover_Transp.Grassland.raster..grassland_2016_02.tif)
#plot(transp.stack$...........Data.Derived_Data.Land_Cover_Transp.Grassland.raster..grassland_2016_06.tif)

#-------------------------------------------------------------------------------
# Cropland ------

#annual turnover/storage
cropland_annual_stack = import_cumulative_transp('Cropland')
ecoregion = 'cropland'
reference_raster = cropland_annual_stack
source('0X_annual_turnover_script.R')
rm(cropland_annual_stack)

#seasonal turnover/storage
cropland_seasonal <- import_monthly_transp_2('Cropland')
ecoregion = 'cropland'
transp.stack = cropland_seasonal
source('test_loop.R')
rm(transp.stack,cropland_seasonal)

#minimum turnover and pixel sample size (# months of data)
cropland_minimum <- import_monthly_transp_2('Cropland')
ecoregion = 'cropland'
transp.stack = cropland_minimum
source('0X_minimum_transit_script.R')
rm(cropland_minimum,transp.stack)

# summary(cropland_seasonal$...........Data.Derived_Data.Land_Cover_Transp.Cropland.raster..cropland_2016_02.tif)
# outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
# ecoregion_dir <- dir(outfile, full.names = T)


#-------------------------------------------------------------------------------
# Tundra ------

#annual turnover/storage
tundra_annual_stack = import_cumulative_transp('Tundra')
ecoregion = 'tundra'
reference_raster = tundra_annual_stack
source('0X_annual_turnover_script.R')
rm(tundra_annual_stack)

#seasonal turnover/storage
tundra_seasonal <- import_monthly_transp_2('Tundra')
ecoregion = 'tundra'
transp.stack = tundra_seasonal
source('test_loop.R')
rm(transp.stack,tundra_seasonal)

#minimum turnover and pixel sample size (# months of data)
tundra_minimum <- import_monthly_transp_2('Tundra')
ecoregion = 'tundra'
transp.stack = tundra_minimum
source('0X_minimum_transit_script.R')
rm(tundra_minimum,transp.stack)

#plot(transp.stack$...........Data.Derived_Data.Land_Cover_Transp.Tundra.raster..tundra_2016_07.tif)


#-------------------------------------------------------------------------------
# Forest ------

#annual turnover/storage
forest_annual_stack = import_cumulative_transp('Forest')
ecoregion = 'forest'
reference_raster = forest_annual_stack
source('0X_annual_turnover_script.R')
rm(forest_annual_stack)

#seasonal turnover/storage
forest_seasonal <- import_monthly_transp_2('Forest')
ecoregion = 'forest'
transp.stack = forest_seasonal
source('test_loop.R')
rm(forest_seasonal,transp.stack)

#minimum turnover and pixel sample size (# months of data)
forest_minimum <- import_monthly_transp_2('Forest')
ecoregion = 'forest'
transp.stack = forest_minimum
source('0X_minimum_transit_script.R')
rm(forest_minimum,transp.stack)


#-------------------------------------------------------------------------------
# Shrubland -----

#annual turnover/storage
shrubland_annual_stack = import_cumulative_transp('Shrubland')
ecoregion = 'shrubland'
reference_raster = shrubland_annual_stack
source('0X_annual_turnover_script.R')
rm(shrubland_annual_stack)

#seasonal turnover/storage
shrubland_seasonal <- import_monthly_transp_2('Shrubland')
ecoregion = 'shrubland'
transp.stack = shrubland_seasonal
source('test_loop.R')
rm(shrubland_seasonal,transp.stack)

#minimum turnover and pixel sample size (# months of data)
shrubland_minimum <- import_monthly_transp_2('Shrubland')
ecoregion = 'shrubland'
transp.stack = shrubland_minimum
source('0X_minimum_transit_script.R')
rm(shrubland_minimum,transp.stack)


#-------------------------------------------------------------------------------
# Barren -----

#annual turnover/storage
barren_annual_stack = import_cumulative_transp('Barren')
ecoregion = 'barren'
reference_raster = barren_annual_stack
source('0X_annual_turnover_script.R')
rm(barren_annual_stack)


# Global (maybe delete) ----

regions <- c('cropland','shrubland','forest','tundra','grassland')

#annual turnover and storage
annual_list_storage <- list()
for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  
  # ecoregion_raster <-
  #   raster(paste0('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_',ecoregion,'_unfiltered.tif'))
  
  ecoregion_raster <-
    raster(paste0('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_',ecoregion,'_unfiltered.tif'))
  
  annual_list_storage[[i]] <- ecoregion_raster
  
}

annual_list[[1]]

global_storage_raster = raster::merge(annual_list_storage[[1]],annual_list_storage[[2]],
                                      annual_list_storage[[3]],annual_list_storage[[4]],
                                      annual_list_storage[[5]])

# plot(global_storage_raster)
# hist(global_raster$layer)
# summary(global_turnover_raster)

writeRaster(global_storage_raster,
            './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif',
            overwrite=T)

#seasonal turnover



#-------------------------------------------------------------------------------
#cleanup ----

rm(regionraster,average_vwc_regions,reference_raster,stack.test,transit.regions,
   transit.regions.df,transit.regions.df.unfiltered.raster,vwc.region)

