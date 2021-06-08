
# generate monthly storage and canopy transpiration data frames for each
# land cover type

# grasslands ------

file.id<-c(1:25)
regions<-c('Grasslands') 

for(i in file.id[10:21]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_turnover_from_water_content(region=ecoregion,x=i,veg='herb')
    test<- do.call("rbind", test)
    test<-get_year_month_column_smap_et_9km(test)
    test<-test[c(1,2,3,4,6,7)]
    #summary(test)
    
    # write to file
    year<-mean(as.numeric(as.character(test$year)))
    month<-mean(as.numeric(as.character(test$month)))
    
    #just poa WC
    #outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    
    # #all herb WC
    # outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/wc_3/",ecoregion,"_",year,"_",month,".csv")
    # 
    
    #mostly herb WC
    outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/wc_4/",ecoregion,"_",year,"_",month,".csv")
    
    
    write.csv(test, outfile, row.names = F)
    
  }
  
}


#-------------------------------------------------------------------------------
# forests -----
file.id<-c(1:25)
regions<-c('Forest') 

for(i in file.id[10:21]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_turnover_from_water_content(region=ecoregion,x=i,veg='woody')
    test<- do.call("rbind", test)
    test<-get_year_month_column_smap_et_9km(test)
    test<-test[c(1,2,3,4,6,7)]
    #summary(test)
    
    # write to file
    year<-mean(as.numeric(as.character(test$year)))
    month<-mean(as.numeric(as.character(test$month)))
    outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    write.csv(test, outfile, row.names = F)
    
  }
  
}



#-------------------------------------------------------------------------------
# tundra ------

file.id<-c(1:25)
regions<-c('Tundra') 

for(i in file.id[10:21]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    #test<-get_land_cover_turnover_from_water_content(region="Tundra",x=1,veg='mixed')
    test<-get_land_cover_turnover_from_water_content(region=ecoregion,x=i,veg='mixed')
    test<- do.call("rbind", test)
    test<-get_year_month_column_smap_et_9km(test)
    test<-test[c(1,2,3,4,6,7)]
    #summary(test)
    
    # write to file
    year<-mean(as.numeric(as.character(test$year)))
    month<-mean(as.numeric(as.character(test$month)))
    outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    write.csv(test, outfile, row.names = F)
    
  }
  
}

# test<-get_land_cover_turnover_from_water_content(region='Grasslands',
#                                                  x=1,
#                                                  veg='herb')

#-------------------------------------------------------------------------------
# shrublands ------

file.id<-c(1:25)
regions<-c('Shrubland') 

for(i in file.id[10:21]){
  
  for(j in 1:length(regions)){
    
    #set region ID
    ecoregion <- regions[j]
    
    #get data to proper format
    test<-get_land_cover_turnover_from_water_content(region=ecoregion,x=i,veg='woody')
    test<- do.call("rbind", test)
    test<-get_year_month_column_smap_et_9km(test)
    test<-test[c(1,2,3,4,6,7)]
    #summary(test)
    
    # write to file
    year<-mean(as.numeric(as.character(test$year)))
    month<-mean(as.numeric(as.character(test$month)))
    outfile <- paste0('./../../Data/Derived_Data/Regional_water_storage_and_T/Land_Cover/',ecoregion,"/",ecoregion,"_",year,"_",month,".csv")
    write.csv(test, outfile, row.names = F)
    
  }
  
}