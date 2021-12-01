
# Estimates turnover times and their intra-annual variation 

# estimate turnover time for the year 2016 ----

#
#

# for grasslands:

#remove row ID
rownames(test.grassland) <-NULL
#summary(test.grassland.grassland)

# #focus on year 2016
# test.grassland<- test.grassland%>%
#   dplyr::filter(year == 2016)

#summary(test.grassland)

# Get annual turnover 

#get cumulative annual T
test.grassland.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test.grassland)
#head(test.grassland.annual.cumulative.T)

#estimate T per day
test.grassland.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.grassland.annual.cumulative.T$canopy_transpiration_mm_m2)/365
test.grassland.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.grassland.annual.cumulative.T$canopy_transpiration_mm_m2,2)
#head(test.grassland.annual.cumulative.T)

# get rid of pixels where T is zero
test.grassland.annual.cumulative.T <- test.grassland.annual.cumulative.T  %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0)

#estimate annual turnover
test.grassland.annual.cumulative.T$cumulative.annual.turnover <- 
  test.grassland.annual.cumulative.T$water_storage_mm_m2/test.grassland.annual.cumulative.T$canopy_transpiration_mm_m2

# # filter out extreme values
# high<-as.numeric(quantile(test.grassland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
# low<-as.numeric(quantile(test.grassland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))
# 
# test.filter.cumulative.grasslands <- test.grassland.annual.cumulative.T %>%
#   dplyr::filter(cumulative.annual.turnover < high) %>%
#   dplyr::filter(cumulative.annual.turnover > low)

#try without outlier removal
test.filter.cumulative.grasslands <- test.grassland.annual.cumulative.T
rm(test.grassland.annual.cumulative.T)

test.filter.cumulative.grasslands$vegetation <- 'Grassland'

#
#

# for forests:

#remove row ID
rownames(test.forest) <-NULL

#focus on year 2016
test.forest <- test.forest %>%
  dplyr::filter(year == 2016)

# Get annual turnover 

#get cumulative annual T
test.forest.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test.forest)
#head(test.forest.annual.cumulative.T)

#estimate T per day
test.forest.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.forest.annual.cumulative.T$canopy_transpiration_mm_m2)/365
test.forest.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.forest.annual.cumulative.T$canopy_transpiration_mm_m2,2)
#head(test.forest.annual.cumulative.T)

# get rid of pixels where T is zero
test.forest.annual.cumulative.T <- test.forest.annual.cumulative.T  %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0)

#estimate annual turnover
test.forest.annual.cumulative.T$cumulative.annual.turnover <- 
  test.forest.annual.cumulative.T$water_storage_mm_m2/test.forest.annual.cumulative.T$canopy_transpiration_mm_m2

# filter out extreme values
high<-as.numeric(quantile(test.forest.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
low<-as.numeric(quantile(test.forest.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))

test.filter.cumulative.forests <- test.forest.annual.cumulative.T %>%
  dplyr::filter(cumulative.annual.turnover < high) %>%
  dplyr::filter(cumulative.annual.turnover > low)

rm(test.forest.annual.cumulative.T)

test.filter.cumulative.forests$vegetation <- 'Forest'

#
#

# for tundra:

#remove row ID
rownames(test.tundra) <-NULL

#focus on year 2016
test.tundra <- test.tundra %>%
  dplyr::filter(year == 2016)

# Get annual turnover 

#get cumulative annual T
test.tundra.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test.tundra)
#head(test.tundra.annual.cumulative.T)

#estimate T per day
test.tundra.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.tundra.annual.cumulative.T$canopy_transpiration_mm_m2)/365
test.tundra.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.tundra.annual.cumulative.T$canopy_transpiration_mm_m2,2)
#head(test.tundra.annual.cumulative.T)

# get rid of pixels where T is zero
test.tundra.annual.cumulative.T <- test.tundra.annual.cumulative.T  %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0)

#estimate annual turnover
test.tundra.annual.cumulative.T$cumulative.annual.turnover <- 
  test.tundra.annual.cumulative.T$water_storage_mm_m2/test.tundra.annual.cumulative.T$canopy_transpiration_mm_m2

# filter out extreme values
high<-as.numeric(quantile(test.tundra.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
low<-as.numeric(quantile(test.tundra.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))

test.filter.cumulative.tundras <- test.tundra.annual.cumulative.T %>%
  dplyr::filter(cumulative.annual.turnover < high) %>%
  dplyr::filter(cumulative.annual.turnover > low)

rm(test.tundra.annual.cumulative.T)

test.filter.cumulative.tundras$vegetation <- 'Tundra'

##

# for shrublands:

#remove row ID
rownames(test.shrubland) <-NULL

#focus on year 2016
test.shrubland <- test.shrubland %>%
  dplyr::filter(year == 2016)

# Get annual turnover 

#get cumulative annual T
test.shrubland.annual.cumulative.T <- aggregate(canopy_transpiration_mm_m2~x+y + water_storage_mm_m2,sum,data=test.shrubland)
#head(test.shrubland.annual.cumulative.T)

#estimate T per day
test.shrubland.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.shrubland.annual.cumulative.T$canopy_transpiration_mm_m2)/365
test.shrubland.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.shrubland.annual.cumulative.T$canopy_transpiration_mm_m2,2)
#head(test.shrubland.annual.cumulative.T)

# get rid of pixels where T is zero
test.shrubland.annual.cumulative.T <- test.shrubland.annual.cumulative.T  %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0)

#estimate annual turnover
test.shrubland.annual.cumulative.T$cumulative.annual.turnover <- 
  test.shrubland.annual.cumulative.T$water_storage_mm_m2/test.shrubland.annual.cumulative.T$canopy_transpiration_mm_m2

# filter out extreme values
high<-as.numeric(quantile(test.shrubland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
low<-as.numeric(quantile(test.shrubland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))

test.filter.cumulative.shrublands <- test.shrubland.annual.cumulative.T %>%
  dplyr::filter(cumulative.annual.turnover < high) %>%
  dplyr::filter(cumulative.annual.turnover > low)

rm(test.shrubland.annual.cumulative.T)

test.filter.cumulative.shrublands$vegetation <- 'Shrubland'

## Bind all of these:

annual_turnover_by_vegetation <- rbind(test.filter.cumulative.grasslands,test.filter.cumulative.forests,
                                       test.filter.cumulative.shrublands,test.filter.cumulative.tundras)



# double check there are not duplicate pixels #

# annual_turnover_by_vegetation$duplicates <- duplicated(annual_turnover_by_vegetation[c(1,2)])
# duplicate_look <- subset(annual_turnover_by_vegetation,duplicates=='TRUE')

  # R assigns duplicates coarsely relative to the pixel resolution (with multiple
  # decimal points and sometime either the same x or y coordinate). Does not appear
  # appear there are actual pixel duplicates

# Remove excess
# rm(test.filter.cumulative.grasslands,test.filter.cumulative.forests,
#    test.filter.cumulative.shrublands,test.filter.cumulative.tundras)

#head(annual_turnover_by_vegetation)


#plot(rasterFromXYZ(annual_turnover_by_vegetation[c(1,2,5)]),main='Vegetation water turnover (days)')

# need an effective color bar due to high values obscuring interpretation!


#-------------------------------------------------------------------------------
# estimate intra-annual (monthly) variability of turnover for year 2016-----


#
#

# for grasslands:

#merge monthly df with outlier-constrained annual data frame 
grasslands_cv_turnover <- merge(test.grassland,test.filter.cumulative.grasslands[c(1,2,5)],by=c('x','y'))
#head(grasslands_cv_turnover)

#get monthly turnover 
grasslands_cv_turnover$monthly_turnover <- 
  (grasslands_cv_turnover$water_storage_mm_m2/grasslands_cv_turnover$canopy_transpiration_mm_m2)
#head(grasslands_cv_turnover)

grasslands_cv_turnover<-aggregate(monthly_turnover~x+y,cv,data=grasslands_cv_turnover)
#summary(grasslands_cv_turnover)

# filter out extreme values
high.cv<-as.numeric(quantile(grasslands_cv_turnover$monthly_turnover,probs=c(0.95),na.rm=TRUE))
low.cv<-as.numeric(quantile(grasslands_cv_turnover$monthly_turnover,probs=c(0.05),na.rm=TRUE))

grasslands_cv_turnover <- grasslands_cv_turnover %>%
  dplyr::filter(monthly_turnover < high.cv) %>%
  dplyr::filter(monthly_turnover > low.cv)

#summary(grasslands_cv_turnover)
grasslands_cv_turnover$monthly_turnover <- round(grasslands_cv_turnover$monthly_turnover,2)

#plot(rasterFromXYZ(grasslands_cv_turnover))

grasslands_cv_turnover$vegetation <- 'Grassland'

#
#

# for forests:

#merge monthly df with outlier-constrained annual data frame 
forests_cv_turnover <- merge(test.forest,test.filter.cumulative.forests[c(1,2,5)],by=c('x','y'))
#head(forests_cv_turnover)

#get monthly turnover 
forests_cv_turnover$monthly_turnover <- 
  (forests_cv_turnover$water_storage_mm_m2/forests_cv_turnover$canopy_transpiration_mm_m2)
#head(forests_cv_turnover)

forests_cv_turnover<-aggregate(monthly_turnover~x+y,cv,data=forests_cv_turnover)
#summary(forests_cv_turnover)

# filter out extreme values
high.cv<-as.numeric(quantile(forests_cv_turnover$monthly_turnover,probs=c(0.95),na.rm=TRUE))
low.cv<-as.numeric(quantile(forests_cv_turnover$monthly_turnover,probs=c(0.05),na.rm=TRUE))

forests_cv_turnover <- forests_cv_turnover %>%
  dplyr::filter(monthly_turnover < high.cv) %>%
  dplyr::filter(monthly_turnover > low.cv)

#summary(forests_cv_turnover)
forests_cv_turnover$monthly_turnover <- round(forests_cv_turnover$monthly_turnover,2)

#plot(rasterFromXYZ(forests_cv_turnover))

forests_cv_turnover$vegetation <- 'Forest'

#
#

# for tundra:

#merge monthly df with outlier-constrained annual data frame 
tundras_cv_turnover <- merge(test.tundra,test.filter.cumulative.tundras[c(1,2,5)],by=c('x','y'))
#head(tundras_cv_turnover)

#get monthly turnover 
tundras_cv_turnover$monthly_turnover <- 
  (tundras_cv_turnover$water_storage_mm_m2/tundras_cv_turnover$canopy_transpiration_mm_m2)
#head(tundras_cv_turnover)

tundras_cv_turnover<-aggregate(monthly_turnover~x+y,cv,data=tundras_cv_turnover)
#summary(tundras_cv_turnover)

# filter out extreme values
high.cv<-as.numeric(quantile(tundras_cv_turnover$monthly_turnover,probs=c(0.95),na.rm=TRUE))
low.cv<-as.numeric(quantile(tundras_cv_turnover$monthly_turnover,probs=c(0.05),na.rm=TRUE))

tundras_cv_turnover <- tundras_cv_turnover %>%
  dplyr::filter(monthly_turnover < high.cv) %>%
  dplyr::filter(monthly_turnover > low.cv)

#summary(tundras_cv_turnover)
tundras_cv_turnover$monthly_turnover <- round(tundras_cv_turnover$monthly_turnover,2)

#plot(rasterFromXYZ(tundras_cv_turnover))

tundras_cv_turnover$vegetation <- 'Tundra'

#
#

# for shrubland: 


#merge monthly df with outlier-constrained annual data frame 
shrublands_cv_turnover <- merge(test.shrubland,test.filter.cumulative.shrublands[c(1,2,5)],by=c('x','y'))
#head(shrublands_cv_turnover)

#get monthly turnover 
shrublands_cv_turnover$monthly_turnover <- 
  (shrublands_cv_turnover$water_storage_mm_m2/shrublands_cv_turnover$canopy_transpiration_mm_m2)
#head(shrublands_cv_turnover)

shrublands_cv_turnover<-aggregate(monthly_turnover~x+y,cv,data=shrublands_cv_turnover)
#summary(shrublands_cv_turnover)

# filter out extreme values
high.cv<-as.numeric(quantile(shrublands_cv_turnover$monthly_turnover,probs=c(0.95),na.rm=TRUE))
low.cv<-as.numeric(quantile(shrublands_cv_turnover$monthly_turnover,probs=c(0.05),na.rm=TRUE))

shrublands_cv_turnover <- shrublands_cv_turnover %>%
  dplyr::filter(monthly_turnover < high.cv) %>%
  dplyr::filter(monthly_turnover > low.cv)

#summary(shrublands_cv_turnover)
shrublands_cv_turnover$monthly_turnover <- round(shrublands_cv_turnover$monthly_turnover,2)

#plot(rasterFromXYZ(shrublands_cv_turnover))

shrublands_cv_turnover$vegetation <- 'Shrubland'

#
#

# bind them all together

cv_turnover_by_vegetation <- rbind(grasslands_cv_turnover,forests_cv_turnover,
                                   tundras_cv_turnover,shrublands_cv_turnover)
#remove
rm(grasslands_cv_turnover,forests_cv_turnover,
      tundras_cv_turnover,shrublands_cv_turnover)

#head(cv_turnover_by_vegetation)

#plot(rasterFromXYZ(cv_turnover_by_vegetation[c(1,2,3)]),'CV Vegetation water turnover')



#-------------------------------------------------------------------------------
# estimate turnover for seasons (four three-month chunks of 2016) -----


#[c(1,2,5,6)]

seasonal_grassland <- get_seasonal_turnover(test=test.grassland)
seasonal_grassland$vegetation <- 'Grassland'

seasonal_forest <- get_seasonal_turnover(test=test.forest)
seasonal_forest$vegetation <- 'Forest'

seasonal_shrubland <- get_seasonal_turnover(test=test.shrubland)
seasonal_shrubland$vegetation <- 'Shrubland'

seasonal_tundra <- get_seasonal_turnover(test=test.tundra)
seasonal_tundra$vegetation <- 'Tundra'


seasons_by_vegetation <- rbind(seasonal_grassland,seasonal_forest,
                               seasonal_shrubland,seasonal_tundra)