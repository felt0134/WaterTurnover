
#import land cover transp data

#this script important monthly transpiration data for each land voer type
# between December 1 2015 and November 31 2016

#all that is being changed in each section is the name of the land cover type
#this could be turned into a function or a loop, but its whatever

# import grasslands -----
regions<-c('Grassland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  #outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  grassland_raster <- raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif')
  
  for(j in ecoregion_dir[1:12]){
    
    
    #test<-fread(j)
    test = raster(j)
    test = extend(test,extent(grassland_raster))
    #test = data.frame(rasterToPoints(test))
    
    #add year and month columns
    # df <-
    #   data.frame(colnames(test)[3])
    # colnames(df) <- 'val'
    # df$val <- gsub('Cropland_','',df$val)
    # # get year
    # year_val <- substr(df$val, 1, 4)
    # # get month
    # month_val <- substr(df$val, 6, 7)
    # test$year <- year_val
    # test$month <- month_val
    # 
    # colnames(test) <- c('x','y','canopy_transpiration_mm_m2','year','month')
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
# test.grassland<- do.call("rbind", region.list)
# rm(region.list,test)

stack <- stack(region.list)
plot(stack)
summed_stack_grassland <- calc(stack,sum)
plot(summed_stack_grassland)



#-------------------------------------------------------------------------------
# import forests ----

regions<-c('Forest')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/")
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


#-------------------------------------------------------------------------------
# import shrublands ----

regions<-c('Shrubland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/")
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

#-------------------------------------------------------------------------------
# import croplands ----

regions<-c('Cropland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  #outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  crop_raster <- raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif')
  
  for(j in ecoregion_dir[1:12]){
    
    
    #test<-fread(j)
    test = raster(j)
    test = extend(test,extent(crop_raster))
    #test = data.frame(rasterToPoints(test))

    #add year and month columns
    # df <-
    #   data.frame(colnames(test)[3])
    # colnames(df) <- 'val'
    # df$val <- gsub('Cropland_','',df$val)
    # # get year
    # year_val <- substr(df$val, 1, 4)
    # # get month
    # month_val <- substr(df$val, 6, 7)
    # test$year <- year_val
    # test$month <- month_val
    # 
    # colnames(test) <- c('x','y','canopy_transpiration_mm_m2','year','month')
    region.list[[j]] <- test
    
    
  }}


#make into data frame
# test.cropland<- do.call("rbind", region.list)
# rm(region.list,test)

stack <- stack(region.list)
#plot(stack)
summed_stack_cropland <- calc(stack,sum)
#plot(summed_stack_cropland)

#-------------------------------------------------------------------------------
# import tundra ----

regions<-c('Tundra')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  #outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/")
  #ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  outfile <- paste0('./../../../Data/Derived_Data/Land_Cover_Transp/',ecoregion,"/raster/")
  ecoregion_dir <- dir(outfile, full.names = T)
  ecoregion_dir <- ecoregion_dir[-5] #remove December 2016 (X2 check before you run this)
  region.list <- list()
  tundra_raster <- raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif')
  
  
  for(j in ecoregion_dir[1:12]){
    
    
    #test<-fread(j)
    test = raster(j)
    test = extend(test,extent(tundra_raster))
    #test = data.frame(rasterToPoints(test))
    
    #test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
# test.tundra<- do.call("rbind", region.list)
# rm(region.list,test)

stack <- stack(region.list)
#plot(stack)
summed_stack_tundra <- calc(stack,sum)
#plot(summed_stack_cropland)




