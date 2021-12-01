#load VWC data

outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,3,4,17)] #remove december 2016

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
rownames(test.vwc) = NULL
# head(test.vwc)
# unique(test.vwc$month)

months<- c('january','february','march','april','may','june','july','august',
                'september','october','november','december')

months.list<-list()

for(i in months){
  
  test_turnover_function<-get_monthly_turnover_2(i)
  
  test_turnover_function$month <- i
  
  months.list[[i]] <- test_turnover_function
  
  
}

monthly.df <- do.call('rbind',months.list)
#head(monthly.df)
#rm(months.list)

#get rid of inf values
monthly.df.no.inf <- monthly.df %>%
  filter(!turnover_days == 'Inf')



# TRANSIT #

#get minimum transit for each pixel
df.min <- aggregate(turnover_days~x+y,min,data=monthly.df.no.inf)
#summary(df.min)

#get range of transit for each pixel
df.min_max <- aggregate(turnover_days~x+y,min_max,data=monthly.df.no.inf)
#summary(df.min_max)

#get CV of transit for each pixel
df.cv <- aggregate(turnover_days~x+y,cv,data=monthly.df.no.inf)
#summary(df.cv)

#get uncertainty of transit (summing in quadrature)
uncert_transit <- error_prop_division(monthly.df.no.inf)

#get sample size for each pixel (not including inf)
sample.size.df <- aggregate(turnover_days~x+y,length,data=monthly.df.no.inf)
#summary(sample.size.df)



#save to file

#transit:

# #minimum transit time
minimum_transit <- rasterFromXYZ(df.min)
crs(minimum_transit) <- '+proj=longlat +datum=WGS84'
#plot(minimum_transit)
#summary(minimum_transit)
writeRaster(minimum_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Minimum/VWC_',ecoregion,'_minimum_transit.tif'),
            overwrite=TRUE)

#range of transit
range_transit <- rasterFromXYZ(df.min_max)
crs(range_transit) <- '+proj=longlat +datum=WGS84'
#plot(range_transit)
#summary(range_transit)
writeRaster(range_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Range/VWC_',ecoregion,'_range_transit.tif'),
            overwrite=TRUE)

#transit sample size (# of months)
sample.size <- rasterFromXYZ(sample.size.df)
crs(sample.size) <- '+proj=longlat +datum=WGS84'
#plot(sample.size)
#summary(sample.size)
writeRaster(sample.size,
            paste0('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_',ecoregion,'_transit_sample_size.tif'),
            overwrite=TRUE)
#rm(sample.size)

# CV of transit
cv_transit <- rasterFromXYZ(df.cv)
crs(cv_transit) <- '+proj=longlat +datum=WGS84'
#plot(cv_transit)
#summary(cv_transit)
writeRaster(cv_transit,
            paste0('./../../../Data/Derived_Data/Turnover/CV/VWC_',ecoregion,'_transit_CV.tif'),
            overwrite=TRUE)

#uncertainty of transit in quadrature
#plot(uncert_transit)
writeRaster(uncert_transit,
            paste0('./../../../Data/Derived_Data/Turnover/Uncertainty/VWC_',ecoregion,'_transit_uncertainty.tif'),
            overwrite=TRUE)



#
#


# STORAGE #


#range of storage
df.min_max_storage <- aggregate(storage_mm~x+y,min_max,data=monthly.df)

#CV of storage
df.cv_storage <- aggregate(storage_mm~x+y,cv,data=monthly.df)

#save storage to file

#range of storage
range_storage <- rasterFromXYZ(df.min_max_storage)
crs(range_storage) <- '+proj=longlat +datum=WGS84'
#plot(range_storage)
#summary(range_storage)
writeRaster(range_storage,
            paste0('./../../../Data/Derived_Data/VWC/Range/VWC_',ecoregion,'_range_storage.tif'),
            overwrite=TRUE)


#CV of storage
cv_storage <- rasterFromXYZ(df.cv_storage)
crs(cv_storage) <- '+proj=longlat +datum=WGS84'
#plot(cv_storage)
#summary(cv_storage)
writeRaster(cv_storage,
            paste0('./../../../Data/Derived_Data/VWC/CV/VWC_',ecoregion,'_cv_storage.tif'),
            overwrite=TRUE)




#source('01_Run_Everything.R')