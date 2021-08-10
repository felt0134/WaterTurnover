# uncertainty propagation

#standard error
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


# num = numerator = storage
# den = denominator = transpiration

error_prop_division <- function(num,den){
  
  #get standard error and relativze by the mean
  se.storage <- (stderr(num))/mean(num)
  se.transp <- (stderr(den))/mean(den)
  
  se.storage.square
  
  uncert <- sqrt((se.storage^2) + (se.transp(2)))
  
  return(uncert)
  
  
}



#standard error divided by mean
stderr_relative <- function(x) {
  
  rel<-stderr(x)
  rel <- rel/mean(x)
  return(rel)
  
}

error_prop_division <- function(dataset){
  
  #df <- as.data.frame(dataset)
  
  #get standard error and divide by the mean
  se.storage.ag <- aggregate(layer~x+y,stderr_relative,data=dataset)
  se.transp <- aggregate(canopy_transpiration_mm_m2~x+y,stderr_relative,data=dataset)
  
  merge.num.den <- merge(se.storage.ag,se.transp,by=c('x','y'))
  
  merge.num.den$uncertainty <- sqrt(((merge.num.den$layer)^2)) + 
    (((merge.num.den$canopy_transpiration_mm_m2)^2))
  
  merge.num.den<-rasterFromXYZ(merge.num.den[c(1,2,5)])
  crs(merge.num.den) <- '+proj=longlat +datum=WGS84'
  
  return(merge.num.den)
  
  
}

test.prop <- error_prop_division(months.grasslands.df)
head(test.prop)
plot(rasterFromXYZ(test.prop))
hist(test.prop$uncertainty)

head(months.grasslands.df)

se.storage.ag.test <- aggregate(layer~x+y,stderr_relative,data=months.grasslands.df)
se.transp.test <- aggregate(canopy_transpiration_mm_m2~x+y,stderr_relative,data=months.grasslands.df)

merge.num.den.test <- merge(se.storage.ag.test,se.transp.test,by=c('x','y'))

merge.num.den.test$uncertainty <- sqrt(((merge.num.den.test$layer)^2)) + 
  (((merge.num.den.test$canopy_transpiration_mm_m2)^2))


merge.num.den.test <- merge(merge.num.den.test[c(1,2,5)],months.grasslands.df,by=c('x','y'))
head(merge.num.den.test)
summary(merge.num.den.test)
merge.num.den.test.raster <- rasterFromXYZ(merge.num.den.test[c(1,2,3)])
crs(merge.num.den.test.raster) <- '+proj=longlat +datum=WGS84'
plot(merge.num.den.test.raster)

grasslands_annual<-raster( './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')

#resample to grasslands
merge.num.den.test.raster <- resample(merge.num.den.test.raster,grasslands_annual)

#shift to dataframe
grasslands_annual<-data.frame(rasterToPoints(grasslands_annual))
head(grasslands_annual)
summary(grasslands_annual)

merge.num.den.test <- merge(data.frame(rasterToPoints(grasslands_annual)),
                            data.frame(rasterToPoints(merge.num.den.test.raster)),by=c('x','y'))
head(merge.num.den.test)
merge.num.den.test$uncertainty <- 
  merge.num.den.test$annual_transit_vwc_grassland_unfiltered*merge.num.den.test$uncertainty

plot(rasterFromXYZ(merge.num.den.test[c(1,2,4)]))
summary(merge.num.den.test)

plot(merge.num.den.test$annual_transit_vwc_grassland_unfiltered,merge.num.den.test$uncertainty)

filtered <- merge.num.den.test %>%
  dplyr::filter(annual_transit_vwc_grassland_unfiltered<20)
plot(rasterFromXYZ(filtered[c(1,2,4)]))
  

