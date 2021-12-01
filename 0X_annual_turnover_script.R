
#generate annual storage and annual turnover and save each raster to file

test.region.cumulative.transp <- data.frame(rasterToPoints(reference_raster))
colnames(test.region.cumulative.transp) <- c('x','y','canopy_transpiration_mm_m2')
head(test.region.cumulative.transp)

# get rid of pixels where cumulative T is zero
test.region.cumulative.transp <- test.region.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 1)
#plot(rasterFromXYZ(test.region.cumulative.transp))

#estimate daily T
test.region.cumulative.transp$canopy_transpiration_mm_m2 <- test.region.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.region.cumulative.transp)

regionraster<-rasterFromXYZ(test.region.cumulative.transp)
#plot(regionraster)

rm(test.region.cumulative.transp)

#load in region VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,3,4,17)] #remove december 2016 and other extras

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

test.vwc <- resample(test.vwc,regionraster)
vwc.region <- mask(test.vwc,regionraster)
rm(test.vwc)
plot(vwc.region)

#try to stack them
stack.test<- raster::stack(vwc.region,regionraster)
#plot(stack.test)

#create and save annual storage
average_vwc_regions <- stack.test$layer
crs(average_vwc_regions) <- '+proj=longlat +datum=WGS84'
#plot(average_vwc_regions)
writeRaster(average_vwc_regions,
            paste0('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2

transit.regions <- stack.test$new
plot(transit.regions)
summary(transit.regions)
summary(rasterToPoints(transit.regions))

#save the unfiltered turnover raster
crs(transit.regions) <- '+proj=longlat +datum=WGS84'
#plot(transit.regions.df.unfiltered.raster)
writeRaster(transit.regions,
            paste0('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_',ecoregion,'_unfiltered.tif'),
            overwrite=T)


#source('01_Run_Everything.R')
#done 