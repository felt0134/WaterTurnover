
# Estimates turnover times and their intra-annual variation 

# estimate turnover time ----

# for grasslands:

#remove row ID
rownames(test.grassland) <-NULL
#summary(test.grassland.grassland)

#focus on year 2016
test.grassland<- test.grassland%>%
  dplyr::filter(year == 2016)

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

# filter out extreme values
high<-as.numeric(quantile(test.grassland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
low<-as.numeric(quantile(test.grassland.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))

test.filter.cumulative.grasslands <- test.grassland.annual.cumulative.T %>%
  dplyr::filter(cumulative.annual.turnover < high) %>%
  dplyr::filter(cumulative.annual.turnover > low)

rm(test.grassland.annual.cumulative.T)

test.filter.cumulative.grasslands$vegetation <- 'grassland'

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
#head(test.annual.cumulative.T)

#estimate T per day
test.forest.annual.cumulative.T$canopy_transpiration_mm_m2 <- (test.forest.annual.cumulative.T$canopy_transpiration_mm_m2)/365
test.forest.annual.cumulative.T$canopy_transpiration_mm_m2 <- round(test.forest.annual.cumulative.T$canopy_transpiration_mm_m2,2)
#head(test.forest.annual.cumulative.T)

# get rid of pixels where T is zero
test.annual.cumulative.T <- test.annual.cumulative.T  %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 0)

#estimate annual turnover
test.forest.annual.cumulative.T$cumulative.annual.turnover <- 
  test.forest.annual.cumulative.T$water_storage_mm_m2/test.annual.cumulative.T$canopy_transpiration_mm_m2

# filter out extreme values
high<-as.numeric(quantile(test.forest.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.95)))
low<-as.numeric(quantile(test.forest.annual.cumulative.T$cumulative.annual.turnover,probs=c(0.05)))

test.filter.cumulative.forests <- test.forest.annual.cumulative.T %>%
  dplyr::filter(cumulative.annual.turnover < high) %>%
  dplyr::filter(cumulative.annual.turnover > low)

rm(test.forest.annual.cumulative.T)

#
#

# need to do the rest of the veg types



