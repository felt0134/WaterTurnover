# Getting the minimum transit time, pixel sample size, and do error propagation
# of dividing storage by T

source('05_Import_Storage_Transp_Data.R')
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)

#load VWC data-

outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,3,16)] #remove december 2016

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

# test.grassland.january <- get_monthly_storage_VWC(month='january',land_cover='grassland')
# head(test.grassland.january)
# test.grassland.january <- rasterFromXYZ(test.grassland.january[c(1,2,5)])
# plot(test.grassland.january)


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


#do error propagation: sum in quadrature

# grassland.prop <- error_prop_division(months.grasslands.df)
# plot(grassland.prop)
# writeRaster(grassland.prop,
#             './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif',
#             overwrite=TRUE)

#do uncertainty estimate: min/max approach sensu cooley et al.

grassland.uncert.max <- transit_uncert_max(x=months.grasslands.df)

writeRaster(grassland.uncert.max,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_grasslands_max.tif',
            overwrite=TRUE)

grassland.uncert.min <- transit_uncert_min(x=months.grasslands.df)

writeRaster(grassland.uncert.min,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_grasslands_min.tif',
            overwrite=TRUE)

#get minimum turnover time for each pixel

# grasslands_minimum_transit<- aggregate(turnover~x+y,min,data=months.grasslands.df)
# grasslands_minimum_transit <- rasterFromXYZ(grasslands_minimum_transit)
# crs(grasslands_minimum_transit) <- '+proj=longlat +datum=WGS84'
# #plot(grasslands_minimum_transit)
# #summary(grasslands_minimum_transit)
# writeRaster(grasslands_minimum_transit,
#             './../../../Data/Derived_Data/Turnover/Minimum/VWC_grasslands_minimum_transit.tif',
#             overwrite=TRUE)


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

#do error propagation

# forest.prop <- error_prop_division(months.forests.df)
# plot(forest.prop)
# writeRaster(forest.prop,
#             './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_forests_quadrature_rel.tif',
#             overwrite=TRUE)

forest.uncert.max <- transit_uncert_max(x=months.forests.df)

writeRaster(forest.uncert.max,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_forests_max.tif',
            overwrite=TRUE)

forest.uncert.min <- transit_uncert_min(x=months.forests.df)

writeRaster(forest.uncert.min,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_forests_min.tif',
            overwrite=TRUE)


#get minimum turnover time for each pixel
# forests_minimum_transit<- aggregate(turnover~x+y,min,data=months.forests.df)
# forests_minimum_transit <- rasterFromXYZ(forests_minimum_transit)
# crs(forests_minimum_transit) <- '+proj=longlat +datum=WGS84'
# #plot(forests_minimum_transit)
# #summary(forests_minimum_transit)
# writeRaster(forests_minimum_transit,
#             './../../../Data/Derived_Data/Turnover/Minimum/VWC_forests_minimum_transit.tif',
#             overwrite=TRUE)

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

#do error propagation

# shrubland.prop <- error_prop_division(months.shrublands.df)
# writeRaster(shrubland.prop,
#             './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_shrublands_quadrature_rel.tif',
#             overwrite=TRUE)

#minimum to maximum uncertainty 
shrubland.uncert.max <- transit_uncert_max(x=months.shrublands.df)

writeRaster(shrubland.uncert.max,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_shrublands_max.tif',
            overwrite=TRUE)

shrubland.uncert.min <- transit_uncert_min(x=months.shrublands.df)

writeRaster(shrubland.uncert.min,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_shrublands_min.tif',
            overwrite=TRUE)


#get minimum turnover time for each pixel
# shrublands_minimum_transit<- aggregate(turnover~x+y,min,data=months.shrublands.df)
# shrublands_minimum_transit <- rasterFromXYZ(shrublands_minimum_transit)
# crs(shrublands_minimum_transit) <- '+proj=longlat +datum=WGS84'
# #plot(shrublands_minimum_transit)
# #summary(shrublands_minimum_transit)
# writeRaster(shrublands_minimum_transit,
#             './../../../Data/Derived_Data/Turnover/Minimum/VWC_shrublands_minimum_transit.tif',
#             overwrite=TRUE)

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
#head(months.croplands.df)

#get sample size for each pixel
# croplands_sample_size <- aggregate(turnover~x+y,length,data=months.croplands.df)
# croplands_sample_size <- rasterFromXYZ(croplands_sample_size)
# crs(croplands_sample_size) <- '+proj=longlat +datum=WGS84'
# #plot(croplands_sample_size)
# writeRaster(croplands_sample_size,
#             './../../../Data/Derived_Data/Sample_Sizes/VWC_croplands_transit_sample_size.tif',
#             overwrite=TRUE)
# rm(croplands_sample_size)

#do error propagation:

# cropland.prop <- error_prop_division(months.croplands.df)
# summary(cropland.prop)
# writeRaster(cropland.prop,
#             './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_croplands_quadrature_rel.tif',
#             overwrite=TRUE)

#minimum to maximum uncertainty:

cropland.uncert.max <- transit_uncert_max(x=months.croplands.df)

writeRaster(cropland.uncert.max,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_croplands_max.tif',
            overwrite=TRUE)

cropland.uncert.min <- transit_uncert_min(x=months.croplands.df)

writeRaster(cropland.uncert.min,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_croplands_min.tif',
            overwrite=TRUE)

#get minimum turnover time for each pixel
# croplands_minimum_transit<- aggregate(turnover~x+y,min,data=months.croplands.df)
# croplands_minimum_transit <- rasterFromXYZ(croplands_minimum_transit)
# crs(croplands_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(croplands_minimum_transit)
#summary(croplands_minimum_transit)
# writeRaster(croplands_minimum_transit,
#             './../../../Data/Derived_Data/Turnover/Minimum/VWC_croplands_minimum_transit.tif',
#             overwrite=TRUE)

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

#do error propagation:

# tundra.prop <- error_prop_division(months.tundras.df)
# writeRaster(tundra.prop,
#             './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_tundras_quadrature_rel.tif',
#             overwrite=TRUE)

#minimum to maximum uncertainty:

tundra.uncert.max <- transit_uncert_max(x=months.tundras.df)

writeRaster(tundra.uncert.max,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_tundras_max.tif',
            overwrite=TRUE)

tundra.uncert.min <- transit_uncert_min(x=months.tundras.df)

writeRaster(tundra.uncert.min,
            './../../../Data/Derived_Data/Uncertainty/min_max/VWC_tundras_min.tif',
            overwrite=TRUE)

#get minimum turnover time for each pixel
# tundras_minimum_transit<- aggregate(turnover~x+y,min,data=months.tundras.df)
# tundras_minimum_transit <- rasterFromXYZ(tundras_minimum_transit)
# crs(tundras_minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(tundras_minimum_transit)
#summary(tundras_minimum_transit)
# writeRaster(tundras_minimum_transit,
#             './../../../Data/Derived_Data/Turnover/Minimum/VWC_tundras_minimum_transit.tif',
#             overwrite=TRUE)


#-------------------------------------------------------------------------------
# combine minimum transit time to produce global raster -----

#minimum transit time
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

#-------------------------------------------------------------------------------
# combine samples sizes (#months) to produce global raster -----

# sample sizes of transit time

grasslands_transit_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_grasslands_transit_sample_size.tif')
forests_transit_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_forests_transit_sample_size.tif')
shrublands_transit_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_shrublands_transit_sample_size.tif')
croplands_transit_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_croplands_transit_sample_size.tif')
tundras_transit_ss <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_tundras_transit_sample_size.tif')

global_transit_ss <- raster::merge(grasslands_transit_ss,forests_transit_ss,
                                   shrublands_transit_ss,croplands_transit_ss,
                                   tundras_transit_ss)
# plot(global_transit_ss)
# summary(global_transit_ss)

writeRaster(global_transit_ss,'./../../../Data/Derived_Data/Sample_Sizes/VWC_global_transit_sample_size.tif',
            overwrite=TRUE)

#-------------------------------------------------------------------------------
# combine uncertainty in quadrature rasters to produce global raster-----

grasslands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif')
forests_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_forests_quadrature_rel.tif')
shrublands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_shrublands_quadrature_rel.tif')
croplands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_croplands_quadrature_rel.tif')
tundra_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_tundras_quadrature_rel.tif')


global_uncertainty <- raster::merge(grasslands_error,forests_error,shrublands_error,
                                    croplands_error,tundra_error)
plot(global_uncertainty)
summary(global_uncertainty)

writeRaster(global_uncertainty,
            './../../../Data/Derived_Data/Uncertainty/quadrature/VWC_global_quadrature_rel.tif',
            overwrite=TRUE)

hist(grasslands_error$VWC_grasslands_quadrature_rel,col='green')
hist(croplands_error$VWC_croplands_quadrature_rel,add=T,col='yellow')
hist(forests_error$VWC_forests_quadrature_rel,add=T,col='blue')
hist(shrublands_error$VWC_shrublands_quadrature_rel,add=T,col='grey')
hist(tundra_error$VWC_tundras_quadrature_rel,add=T,col='gold')


#-------------------------------------------------------------------------------
# combine uncertainty in max-min range rasters to produce global raster ----



