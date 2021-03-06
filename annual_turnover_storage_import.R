
#get annual storage and turnover

#land cover ID
lc_id <- read.csv('./../../Data/land_cover_nsidc_ease2/land_cover_id.csv')
unique(lc_id$group)

#set directories
annual_turnover_filepath <- './../../Data/turnover_from_python/annual/land_cover_csvs/'
annual_turnover_dir <- dir(annual_turnover_filepath, full.names = T)
annual_turnover_dir <- annual_turnover_dir[-c(11,13,15,16,17)] #remove land classes with no data

#now loop through each land cover file and import. truncating by the 90th percentile for figures

#annual turnover time and storage

#import original LC
annual_turnover_list <- list()
for(i in annual_turnover_dir[1:12]) {
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(annual_turnover > 0) %>%
    dplyr::filter(annual_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      './../../Data/turnover_from_python/annual/land_cover_csvs//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  
  annual_turnover_list[[i]] <- lc_filtered
  
}

annual_turnover_lc <- do.call('rbind', annual_turnover_list)
rm(annual_turnover_list)
rownames(annual_turnover_lc) <- NULL

#done

rm(lc_filtered,lc,name)

#only consider pixels with at least 4 replicates/months of data
annual_turnover_lc <- annual_turnover_lc %>%
  dplyr::filter(sample_size > 3)

#quick look
quantile(annual_turnover_lc$annual_turnover,probs=c(0.05,0.5,0.95))

#
