
# import the derived storage and trasp data

# import grasslands data ------

regions<-c('Grasslands')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    

    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.grassland<- do.call("rbind", region.list)
rm(region.list,test)

#-------------------------------------------------------------------------------
# import forests data ------


regions<-c('Forest')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){

    test<-fread(j)
    region.list[[j]] <- test
    
    
  }}

#make into data frame
test.forest<- do.call("rbind", region.list)
rm(region.list,test)




#-------------------------------------------------------------------------------
# import tundra data ------

regions<-c('Tundra')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    
 
    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.tundra<- do.call("rbind", region.list)
rm(region.list,test)


#-------------------------------------------------------------------------------
# import shrubland data ------

regions<-c('Shrubland')

for(i in 1:length(regions)){
  
  ecoregion <- regions[i]
  outfile <- paste0('./../../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/")
  ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
  region.list <- list()
  
  for(j in ecoregion_dir[1:12]){
    

    test<-fread(j)
    region.list[[j]] <- test
    
    
    
  }}

#make into data frame
test.shrubland<- do.call("rbind", region.list)
rm(region.list,test)




