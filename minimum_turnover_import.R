
#minimum turnover import


#land cover ID
lc_id <- read.csv('./../../Data/land_cover_nsidc_ease2/land_cover_id.csv')
unique(lc_id$group)


#set directories
minimum_turnover_filepath <- './../../Data/turnover_from_python/minimum/land_cover_csvs/'
minimum_turnover_dir <- dir(minimum_turnover_filepath, full.names = T)
minimum_turnover_dir <- minimum_turnover_dir[-c(11,13,15,16,17)] #remove land classes with no data

#loop through land cover types
minimum_turnover_list <- list()
for(i in minimum_turnover_dir[1:12]){
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(minimum_turnover > 0) %>%
    dplyr::filter(minimum_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      './../../Data/turnover_from_python/minimum/land_cover_csvs//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  
  minimum_turnover_list[[i]] <- lc_filtered
  
}

minimum_turnover_lc <- do.call('rbind',minimum_turnover_list)
rm(minimum_turnover_list)
rownames(minimum_turnover_lc) <- NULL

#done

