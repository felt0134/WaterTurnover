
# compare VWC from VOD and tissue density -----

#import DF
dry_biomass<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

# convert to mg/hectare
dry_biomass <- dry_biomass/10

#1000000 grams = 1 megagram
dry_biomass<-dry_biomass*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass<-dry_biomass/10000

plot(dry_biomass)

#convert g to mm (in height) per meter squared:


# The logic:
#   
# 1 gram = 1 ml = 1000 mm^3
# That is a volume
# 1 m^2=1000000 mm^2 = 1000mm*1000mm
# Volume = Length*Width*Height
# Therefore
# 1000 mm3 volume = 1000 length*1000width*XXHeight
# 1000/1000000 = .001 mm of water on a m2

dry_biomass<-dry_biomass*.001

#assume water is 0.41 of dry biomass:
#0.41/0.59
dry_biomass<-dry_biomass*0.70


#import VOD->VWC raster (in mm/m^2) 
vwc_from_vod <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#turn to DF
vwc_from_vod_df_2 <- data.frame(rasterToPoints(vwc_from_vod))

#turn biomass raster to df

#resample so the lat/lons match up nicely
dry_biomass_2 <- resample(dry_biomass,vwc_from_vod)

#then merge
dry_biomass_df_2 <- data.frame(rasterToPoints(dry_biomass_2))

#merge them
merge_biomass_vwc <- merge(vwc_from_vod_df_2,dry_biomass_df_2,by=c('x','y'))
head(merge_biomass_vwc)

#calculate difference
merge_biomass_vwc$diff <- (merge_biomass_vwc$aboveground_dry_biomass_density_aggregate_30X - 
                             merge_biomass_vwc$annual_storage_vwc_global_unfiltered)
summary(merge_biomass_vwc)
# median diff of -0.6 mm/m^2

#make to raster
merge_biomass_vwc_raster <- rasterFromXYZ(merge_biomass_vwc[c(1,2,5)])
crs(merge_biomass_vwc_raster) <-  '+proj=longlat +datum=WGS84 +no_defs'

#map out the differences
png('Figures/2016_storage_difference_0.41.png',
    width=8,height=6,units="in",res=300)
plot(merge_biomass_vwc_raster,main='VWC difference (biomass-based minus VOD-based)')
dev.off()

#look at correlation
cor(merge_biomass_vwc$annual_storage_vwc_global_unfiltered,
    merge_biomass_vwc$aboveground_dry_biomass_density_aggregate_30X)
# 0.77

#make correlation plot
#they are highly correlated
library(scales) #needed for alpha command

png('Figures/Supporting/vwc_versus_biomass_0.41.png',
    width=1000,height=1000,res=150)

plot(merge_biomass_vwc$annual_storage_vwc_global_unfiltered,
     merge_biomass_vwc$aboveground_dry_biomass_density_aggregate_30X,
     cex=0.3,col=alpha("grey70",0.5),
     ylab='Biomass-based water storage',xlab='VOD-based water storage')

# Make 1:1 line
abline(a=0,b=1,col='black',lwd=4)
text(15,10,'1:1 line')

dev.off()

#can we compare monthly deviations in water storage?

#try for grasslands
head(merge_biomass_vwc)

#loop it

month_list <- c('january','february','march','april','may','june','july','august',
                'september','october','november','december')
#month_list <- 'january'
store_list<-list()

for( i in month_list){

#create a reference raster to resample to
months.grasslands.df.july<-subset(months.grasslands.df,month==i) 
months.grasslands.df.july.raster<-rasterFromXYZ(months.grasslands.df.july[c(1,2,4)])
#crs(months.grasslands.df.july.raster) <- '+proj=longlat +datum=WGS84'

#resample to match up coordinates, then covert back to a dataframe to merge
merge_biomass_vwc_raster<- rasterFromXYZ(merge_biomass_vwc[c(1,2,3)])
#crs(merge_biomass_vwc_raster) <- '+proj=longlat +datum=WGS84'
merge_biomass_vwc_raster<-resample(merge_biomass_vwc_raster,months.grasslands.df.july.raster)
merge_biomass_vwc_raster<-data.frame(rasterToPoints(merge_biomass_vwc_raster))

months.grasslands.df.july<-data.frame(rasterToPoints(months.grasslands.df.july.raster))
months.grasslands.df.july$month = i

#merge
grasslands_monthly_vwc_biomass_storage <- 
  merge(merge_biomass_vwc_raster,months.grasslands.df.july,by=c('x','y'))

store_list[[i]] <- grasslands_monthly_vwc_biomass_storage
  
}

store_list_df <- do.call('rbind',store_list)
head(store_list_df)
store_list_df$diff <- abs(store_list_df$annual_storage_vwc_global_unfiltered -
                            store_list_df$layer)

storage_deviations <- aggregate(diff~x+y,sd,data=store_list_df)

# Remove NAs
storage_deviations <- storage_deviations %>%
  dplyr::filter(!diff=='NA') 
head(storage_deviations)
summary(storage_deviations)
plot(rasterFromXYZ(storage_deviations))

#stopped here 08/11/2021. The grid become irregular. 
                                                               


# compare global amounts of water in cubic km -----

#now go from mm/m^2 to km cubed
dry_biomass_cubed <- get_km_cubed(dry_biomass)
plot(dry_biomass)

vwc_from_vod_cubed <- get_km_cubed(vwc_from_vod)
plot(vwc_from_vod)

#convert to df to add up
dry_biomass_cubed_df <- data.frame(rasterToPoints(dry_biomass_cubed))
vwc_from_vod_cubed_df <- data.frame(rasterToPoints(vwc_from_vod_cubed))

#compare total values (allowing pixel rep. to vary among rasters):
sum(dry_biomass_cubed_df$aboveground_dry_biomass_density_aggregate_30X)
#8000.837 when we assuming water is 50% fresh mass
# 5601.304 when we assuming water is 41% fresh mass

sum(vwc_from_vod_cubed_df$annual_storage_vwc_global_unfiltered)
#6420.823

#compare total values (for same pixel representation)
vwc_from_vod_cubed_regridded <- resample(vwc_from_vod_cubed,dry_biomass_cubed)

vwc_same_grid_merged <- merge(data.frame(rasterToPoints(vwc_from_vod_cubed_regridded)),
                              data.frame(rasterToPoints(dry_biomass_cubed)),
                              by=c('x','y'))

# compare
sum(vwc_same_grid_merged$annual_storage_vwc_global_unfiltered)
#6623.222
sum(vwc_same_grid_merged$aboveground_dry_biomass_density_aggregate_30X)
#7759.645

#still off by about 1000

#cleanup
#rm()

#resample so one same exact grid before merging:
dry_biomass_resampled <- resample(dry_biomass,vwc_from_vod)
dry_biomass_resampled<-data.frame(rasterToPoints(dry_biomass_df))


# in short: assumming water is 50% of total biomass leads to much higher
# estimates of VWC than VOD

# now compare to the ground-based estimates in forests -----

#import
ground_estimates <- read.csv('./../../../Data/Water_content/woodwater_empirical_greg.csv')
#head(ground_estimates)

#narrow down/rename columns
ground_estimates <-ground_estimates[c('Lat','Long','mean.moisture')]
colnames(ground_estimates) <- c('y','x','moisture.content')

#turn into normally gridded raster
ground_estimates <-ground_estimates[c(2,1,3)]
ground_estimates <- na.exclude(ground_estimates)
ground_estimates <- fix_grid(ground_estimates)
#plot(ground_estimates)

#re-import biomass data
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000

#sinze ground estimates are at coarser resolution, resample to that
dry_biomass_only <- resample(dry_biomass_only,ground_estimates)

#merge them
ground_estimates_df <-merge(data.frame(rasterToPoints(ground_estimates)),
                            data.frame(rasterToPoints(dry_biomass_only)),
                            by=c('x','y'))
#head(ground_estimates_df)
#length(unique(ground_estimates_df$layer)) 

#convert water content to water
ground_estimates_df$veg_water <- 
  ground_estimates_df$layer*ground_estimates_df$aboveground_dry_biomass_density_aggregate_30X

#convert from g/m^2 to mm/m^2
ground_estimates_df$veg_water <- ground_estimates_df$veg_water*.001
ground_estimates_df <- ground_estimates_df[c(1,2,5)]

#re-import VOD-> VWC raster and turn into data frame
vwc_from_vod <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#reample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_estimates)

#merge with ground-based df
ground_estimates_df <- merge(ground_estimates_df,data.frame(rasterToPoints(vwc_from_vod)))

png('Figures/Supporting/vod_versus_groundbased_storage.png',
    width=1000,height=1000,res=150)

plot(annual_storage_vwc_global_unfiltered~veg_water,data=ground_estimates_df,
     xlab='Ground-based water storage',ylab='VOD-based water storage',cex=4.5)
# Make 1:1 line
abline(a=0,b=1,col='black',lwd=3)
text(7,6,'1:1 line')

dev.off()

summary(lm(annual_storage_vwc_global_unfiltered~veg_water,data=ground_estimates_df))

#see the slope of VOD and VWC....
head(ground_estimates_df)

#convert back to vod
ground_estimates_df$vod <- ground_estimates_df$annual_storage_vwc_global_unfiltered*0.11
summary(lm(vod~veg_water,data=ground_estimates_df))
#beta paramter = 0.08


# compare to ground-based estimates in grasslands ------


# Load in poa and herb X2 checked water contet, subset to grasslands
# units of water content are in gH2O/g dry mass

grassland_wc<-read.csv('./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')
head(grassland_wc)
#100 observations

#get to XYZ format
grassland_wc <-grassland_wc[c(2,3,4)]

ground_wc_raster <- fix_grid(grassland_wc)
plot(ground_wc_raster)

#re-import biomass data (repeated code...)
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000

#since ground estimates raster show up at coarser resolution, we resample to that
#this will require aggregation of biomass pixels around the WC pixels
dry_biomass_only_grassland_wc <- resample(dry_biomass_only,ground_wc_raster)
#plot(dry_biomass_only_grassland_wc)

#merge them
ground_estimates_grassland_df <-merge(data.frame(rasterToPoints(ground_wc_raster)),
                            data.frame(rasterToPoints(dry_biomass_only_grassland_wc)),
                            by=c('x','y'))

#dim(ground_estimates_grassland_df)
#still 100 observations.


#convert water content to water (in g/m^2)
ground_estimates_grassland_df$veg_water <- 
  ground_estimates_grassland_df$layer*ground_estimates_grassland_df$aboveground_dry_biomass_density_aggregate_30X

#head(ground_estimates_grassland_df)

#convert to mm/m^2
ground_estimates_grassland_df$veg_water <- ground_estimates_grassland_df$veg_water*0.001
head(ground_estimates_grassland_df)

#re-import VOD-> VWC raster and turn into data frame (repeated code)
vwc_from_vod <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#reample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_wc_raster)

#merge with ground-based df
ground_estimates_grassland_df <- merge(ground_estimates_grassland_df,
                             data.frame(rasterToPoints(vwc_from_vod)))


#merge with ground-based df
ground_estimates_grassland_df <- merge(ground_estimates_grassland_df,data.frame(rasterToPoints(vwc_from_vod)))

#filter out things over 2000 g of aboveground biomass because it is unlikely to be a grassland.
ground_estimates_grassland_df_filtered <- ground_estimates_grassland_df %>%
  dplyr::filter(aboveground_dry_biomass_density_aggregate_30X < 1000)

#dim(ground_estimates_grassland_df_filtered) #46 observations

cor(ground_estimates_grassland_df_filtered$annual_storage_vwc_global_unfiltered,
    ground_estimates_grassland_df_filtered$veg_water)
#0.47


png('Figures/Supporting/vod_versus_groundbased_storage_grassland.png',
    width=1000,height=1000,res=150)

plot(annual_storage_vwc_global_unfiltered~veg_water,data=ground_estimates_grassland_df_filtered,
     xlab='Ground-based water storage',ylab='VOD-based water storage',cex=4.5)
# Make 1:1 line
abline(a=0,b=1,col='black',lwd=3)
text(3.5,4,'1:1 line')

dev.off()


#convert back to vod
ground_estimates_grassland_df_filtered$vod <- ground_estimates_grassland_df_filtered$annual_storage_vwc_global_unfiltered*0.11
summary(lm(vod~veg_water,data=ground_estimates_grassland_df_filtered))
#beta parameter = 0.06


# estimate uncertainty ------


#first do it just for  VWC-based approach based on temporal variation

#relative or fractional basis
#grasslands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif')

#absolute basis (multiply fractional uncertainty by turnover)
global_error_raster <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_global_quadrature_rel.tif')
global_turnover_raster <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')
plot(global_error_raster)

#stack them
global_error_turnover <- stack(global_error_raster,global_turnover_raster)

#get absolute uncertainty 
global_error_turnover$abs.unc <- 
  global_error_turnover$VWC_global_quadrature_rel*global_error_turnover$annual_transit_vwc_global_unfiltered 

summary(global_error_turnover$abs.unc)

#convert to dataframe
global_error_turnover <- data.frame(rasterToPoints(global_error_turnover))
head(global_error_turnover)
summary(global_error_turnover)

#filter out high/extreme values
head(global_error_turnover)
global_error_turnover <- global_error_turnover %>%
  dplyr::filter(annual_transit_vwc_global_unfiltered < 20)
  dplyr::filter(!abs.unc == 'NA')

summary(global_error_turnover)

plot(rasterFromXYZ(global_error_turnover[c(1,2,5)]))
plot(abs.unc~annual_transit_vwc_global_unfiltered,data=global_error_turnover)
hist(global_error_turnover$abs.unc)



# just plot the relative (CV?) 
summary(global_error_raster)

# Make colors
bks<- c(quantile(data.frame(rasterToPoints(global_error_raster))$VWC_global_quadrature_rel, 
                 probs=seq(0.0, 0.95, by=0.1), na.rm = TRUE),2)
#sensitivity=c("purple",'cyan3','green','yellow','orange','red')
sensitivity=c("blue",'lightblue','orange', 'red')
bkcols.sensitivity <- c(colorRampPalette(sensitivity)(length(bks)-1),'brown')
r.range.sens <- round(c(minValue(global_error_raster), maxValue(global_error_raster)),digits=2)

# Update projection
# proj4string(global_truncated) <- CRS("+proj=longlat")
# sensitivity_raster_2<-projectRaster(sensitivity_raster, crs=aea.proj)

png('Figures/global_transit_truncated.png',
    width=2200,height=1000,res=300)


# Set up multi-panel plot
layout(matrix(1:2, ncol=2),pty='s')
par(oma=c(2,2,2,2), mar=c(0.5, 4, 0.5, 4))
?par
#pty='s'
# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.1
?ifelse
plot(global_error_raster$VWC_global_quadrature_rel,breaks = bks,axes=F,box=F,
     col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,main='Observed',cex.main=1,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 5),
                    labels=seq(r.range.sens[1], r.range.sens[2], 5),
                    cex.axis=1.0),
     legend.args=list(text='', side=4, font=10, line=2.5, cex=0.7))
#stopped here 8/11/2021

plot(global_error_raster$VWC_global_quadrature_rel,
     col = ifelse(global_error_raster >1.3,'red','blue'))





#extra
grasslands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif')
grasslands_turnover <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
grasslands_error <- resample(grasslands_error,grasslands_turnover)
grasslands_error_2 <- raster::stack(grasslands_error,grasslands_turnover)
plot(grasslands_error_2)
grasslands_error_2$unncertainty <- 
  grasslands_error_2$VWC_grasslands_quadrature_rel*grasslands_error_2$annual_transit_vwc_grassland_unfiltered
plot(grasslands_error_2)
summary(grasslands_error_2$unncertainty)
summary(grasslands_error_2$annual_transit_vwc_grassland_unfiltered)

grasslands_error_df <- data.frame(rasterToPoints(grasslands_error_2))
head(grasslands_error_df)
plot(unncertainty~annual_transit_vwc_grassland_unfiltered,data=grasslands_error_df)

#filter out high values
grasslands_error_df <- grasslands_error_df %>%
  dplyr::filter(annual_transit_vwc_grassland_unfiltered < 50)
summary(grasslands_error_df)

#summary(lm(unncertainty~annual_transit_vwc_grassland_unfiltered,data=grasslands_error_df))

plot(rasterFromXYZ(grasslands_error_df[c(1,2,5)]))

#now do it based off of monthly differences in VWC versus Biomass-based estimates





# truncate distribution -----

#library(truncdist)

#simply truncate at 95th percentile
get_turncated_dist <- function(land_cover,annual=T){

#get filepath
  
if(annual==T){

filepath<-paste0("./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_",land_cover,"_unfiltered.tif")

}else{
  
  
  filepath<-paste0("./../../../Data/Derived_Data/Turnover/Minimum/VWC_",land_cover,"_minimum_transit.tif")
  
}

#load raster
grasslands_turnover <- raster(filepath)

#convert to dataframe
grasslands_turnover_df <- data.frame(rasterToPoints(grasslands_turnover))
colnames(grasslands_turnover_df) <- c('x','y','turnover')
#head(grasslands_turnover_df)

#turncate right (top 5%)
high<-round(quantile(grasslands_turnover_df$turnover,
               probs=0.95),2)

grasslands_turnover_df_truncate <- grasslands_turnover_df %>%
  dplyr::filter(turnover < high)
grasslands_turnover_df_truncate$cover <- land_cover

return(grasslands_turnover_df_truncate)

}

#annual turnover
land_covers <-c('grassland','forest','tundra','croplands','shrubland')
truncate_list <- list()

for(i in land_covers){
  
  truncate_list[[i]]<-get_turncated_dist(i,annual=T)
  
}

png('Figures/annualized_transit_ecdfs.png',
    width=2500,height=2000,res=300)

plot(ecdf(data.frame(truncate_list[1])$grassland.turnover),
     xlab='Annualized transit time (days)',
     ylab='Probability density',main='',xlim=c(0,60))
plot(ecdf(data.frame(truncate_list[2])$forest.turnover),add=TRUE,col='red')
plot(ecdf(data.frame(truncate_list[3])$tundra.turnover),add=TRUE,col='grey')
plot(ecdf(data.frame(truncate_list[5])$shrubland.turnover),add=TRUE,col='blue')
plot(ecdf(data.frame(truncate_list[4])$croplands.turnover),add=TRUE,col='green')
abline(a=0.5,b=0,lty='dashed',col='grey',lwd=1)
legend(15, 0.30, legend=c("Grasslands","Forests","Tundras","Shrublands","Croplands"),     
       col=c("black", "red",'grey','blue',"green"), lty=1.0,lwd=5,cex=1.1,box.lty=0)

dev.off()

#annual turnover (notice now the names are plural because of filepath)

land_covers_minimum <-c('grasslands','forests','tundras','croplands','shrublands')
truncate_list_minimum <- list()

for(i in land_covers_minimum){
  
  truncate_list_minimum[[i]]<-get_turncated_dist(i,annual=F)
  
}




png('Figures/minimum_transit_ecdfs.png',
    width=2500,height=2000,res=300)

plot(ecdf(data.frame(truncate_list_minimum[1])$grasslands.turnover),xlab='Minimum transit time (days)',
     ylab='Probability density',main='',xlim=c(0,8)) #xlim=c(0,60)
plot(ecdf(data.frame(truncate_list_minimum[2])$forests.turnover),add=TRUE,col='red')
plot(ecdf(data.frame(truncate_list_minimum[3])$tundras.turnover),add=TRUE,col='grey')
plot(ecdf(data.frame(truncate_list_minimum[5])$shrublands.turnover),add=TRUE,col='blue')
plot(ecdf(data.frame(truncate_list_minimum[4])$croplands.turnover),add=TRUE,col='green')
abline(a=0.5,b=0,lty='dashed',col='grey',lwd=1)
legend(4, 0.3, legend=c("Grasslands","Forests","Tundras","Shrublands","Croplands"),     
       col=c("black", "red",'grey','blue',"green"), lty=1.0,lwd=5,cex=1.1,box.lty=0)

dev.off()


#filter uncertainty by truncated dist-------
global_error_raster <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_global_quadrature_rel.tif')

hist(global_error_raster$VWC_global_quadrature_rel)
summary(global_error_raster)

rbind_annual_truncated <- do.call("rbind",truncate_list)
head(rbind_annual_truncated)
dim(rbind_annual_truncated)
rbind_annual_truncated<-rasterFromXYZ(rbind_annual_truncated[c(1,2,3)])

land_covers <-c('grassland','forest','tundra','croplands','shrubland')
trun.annual.list<-list()

for(i in land_covers){

test.trunc <- get_turncated_dist(i,annual=T)
test.trunc <- rasterFromXYZ(test.trunc[c(1,2,3)])
crs(test.trunc) <- '+proj=longlat +datum=WGS84'

trun.annual.list[[i]] <- test.trunc

}

global_truncated<-
  raster::merge(trun.annual.list[1]$grassland,trun.annual.list[2]$forest,
                trun.annual.list[3]$tundra,trun.annual.list[4]$croplands,
                trun.annual.list[5]$shrubland)
plot(global_truncated)
sd(global_error_truncated$VWC_global_quadrature_rel)

global_error_truncated <- mask(global_error_raster,global_truncated)
plot(global_error_truncated)
summary(global_error_truncated)
hist(global_error_truncated$VWC_global_quadrature_rel)

library("rnaturalearth")
library("rnaturalearthdata")
library(ggplot2)
global_error_truncated_df <- rasterToPoints(global_error_truncated)
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) + geom_sf() + 
  geom_point(data= data.frame(global_error_truncated_df),aes(x=x, y=y, color = VWC_global_quadrature_rel),size=0.1) +
  scale_color_gradient2(midpoint = 0.58, low = "blue", mid = "white",
                        high = "red", space = "Lab" )

plot(test.trunc)
head(test.trunc)

#old------
#look at ecdf
hist(grasslands_turnover_df_truncate$turnover)
plot(ecdf(grasslands_turnover_df_truncate$turnover))
summary(grasslands_turnover_df_truncate)

mean_turnover<-mean(grasslands_turnover_df_truncate$annual_transit_vwc_grassland_unfiltered)
sd_turnover<-sd(grasslands_turnover_df_truncate$annual_transit_vwc_grassland_unfiltered)
toy.df<-rnorm(10000,mean=mean_turnover,sd_turnover)
y<-dnorm(toy.df,mean=mean_turnover,sd_turnover)
plot(toy.df,y)

library(truncnorm)
test=dtruncnorm(toy.df,a=0,b=high,mean=mean_turnover,sd=sd_turnover)
plot(test)

#y <- dnorm(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered)
summary(y)
new <- dtrunc(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered,
              spec="norm", a=0, b=high)
summary(new)
plot(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered, 
     new, xlab = "x", ylab = "PDF",xlim=c(0,high))
  
plot(new)


#get normal distribution of PPT
mean_ppt<-mean(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered)
sd_ppt <- sd(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered)
toy.df<-rnorm(10000,mean=mean_ppt,sd_ppt)
summary(toy.df)
y<-dnorm(toy.df,mean=mean_ppt,sd_ppt)
plot(toy.df,y)


#from the paper

x <- seq(-3, 3, by = 0.1)
y1 <- dnorm(x)
y2 <- dtrunc(x, "norm", a = -0.5, b = 0.5, mean = 0, sd = 2)
R> y3 <- dtrunc(x, "norm", a = -1, b = 1, mean = 0, sd = 2)
R> y4 <- dtrunc(x, "norm", a = -2, b = 2, mean = 0, sd = 2)
yrange <- range(y1, y2, y3, y4)
plot(x, y1, type = "l", xlab = "x", ylab = "PDF", xlim = c(-3,
                                                              + 3))
R> lines(x, y2, lty = 2)
R> lines(x, y3, lty = 3)
R> lines(x, y4, lty = 4)

#try to mimic this



cdf <- ecdf(grasslands_turnover_df$annual_transit_vwc_grassland_unfiltered)
plot(cdf)




#-------


#make a plot with land cover types colored and add the points where have data
# for wood water content 

#make some plot of (minimum) transit time with:

# MAT, MAP, aridity, soil moisture

