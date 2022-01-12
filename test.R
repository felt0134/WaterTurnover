
# compare VWC from VOD and tissue density (need to know how we are going to convert) -----

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

water_storage<-dry_biomass*.001

#assume water is 0.41 of dry biomass:
#0.41/0.59 (I don't remember the logic of this)
water_storage<-water_storage*0.70
plot(water_storage)

#or
water_storage<-water_storage*0.41


#import VOD->VWC raster (in mm/m^2) 

#load in annual storage
grasslands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_unfiltered_storge<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')


vwc_from_vod = raster::merge(grasslands_unfiltered_storge,forests_unfiltered_storge,
                        shrublands_unfiltered_storge,tundras_unfiltered_storge,
                        croplands_unfiltered_storge)

plot(vwc_from_vod)

vwc_from_vod_2 <- data.frame(rasterToPoints(vwc_from_vod))
head(storage_df)

#turn to DF
# vwc_from_vod_df_2 <- data.frame(rasterToPoints(vwc_from_vod))

#turn biomass raster to df

#resample so the lat/lons match up nicely
water_storage_2 <- resample(water_storage,vwc_from_vod)

#mask
water_storage_2 = mask(water_storage_2,vwc_from_vod)
plot(water_storage_2)

#stack
stack_storage = stack(water_storage_2,vwc_from_vod)
plot(stack_storage)

#get difference
storage_diff <- stack_storage$layer - stack_storage$aboveground_dry_biomass_density_aggregate_30X
plot(storage_diff)
summary(storage_diff)

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



                                                               


# compare global amounts of water in cubic km -----

#now go from mm/m^2 to km cubed (wait to calculate)
dry_biomass_cubed <- get_km_cubed(dry_biomass)
plot(dry_biomass)

#load vod-based storage data (mm/m^2 of water)
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')
#plot(vwc_from_vod)
vwc_from_vod_cubed <- get_km_cubed_3(vwc_from_vod)

vwc_from_vod_cubed_df <- data.frame(rasterToPoints(vwc_from_vod_cubed))
sum(vwc_from_vod_cubed_df$annual_storage_vwc_global_unfiltered)
#612.54


#3000^2
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

# compare to the ground-based estimates in forests.grasslands,tundra -----

#import
ground_estimates <- read.csv('./../../../woodwater/Data/site_WC_estimates.csv')
#head(ground_estimates)

#use only cross-checked sites
ground_estimates <- subset(ground_estimates,Exclude=='No')
unique(ground_estimates$Exclude)

#narrow down/rename columns
ground_estimates <-ground_estimates[c('Land.Cover.Type', 'Lat','Long','mean.moisture')]
colnames(ground_estimates) <- c('Land.Cover.Type','y','x','moisture.content')

head(ground_estimates)

#turn into normally gridded raster
ground_estimates <-ground_estimates[c(1,3,2,4)]
ground_estimates <- na.exclude(ground_estimates)

#forest
ground_estimates_forest <- subset(ground_estimates,Land.Cover.Type=='Forest')


#re-import biomass data
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000


#loop through
df_list = list()

for(i in 1:nrow(isotope)){
  
  df <- isotope[i,]
  
  coord <- cbind(df[4],df[3]) #get coordinate
  
  transit.isotope <- df[7]
  
  season <- df$season
  
  if(season=='Winter'){
    
    value <- extract(Winter_global,coord)
    
  }else if(season=='Spring'){
    
    value <- extract(Spring_global,coord)
    
    
  }else if(season=='Summer'){
    
    value <- extract(Summer_global,coord)
    
  }else if(season=='Fall'){
    
    value <- extract(Fall_global,coord)
    
    
  }
  
  
  
  df.2 <- data.frame(coord)
  df.2$transit.vod <- value
  df.2$transit.isotope <- as.numeric(transit.isotope)
  
  df_list[[i]] <-df.2
  
  
}

transit.df.2 <- data.frame(do.call('rbind',df_list))




#since ground estimates are at coarser resolution, resample to that
dry_biomass_only <- resample(dry_biomass_only,ground_estimates_forest)

#merge them
ground_estimates_forest_df <-merge(data.frame(rasterToPoints(ground_estimates_forest)),
                            data.frame(rasterToPoints(dry_biomass_only)),
                            by=c('x','y'))
#head(ground_estimates_forest_df)
#length(unique(ground_estimates_df$layer)) 

#convert water content to water in g/m^2
ground_estimates_forest_df$veg_water <- 
  ground_estimates_forest_df$layer*ground_estimates_forest_df$aboveground_dry_biomass_density_aggregate_30X

#convert from g/m^2 to mm/m^2
ground_estimates_forest_df$veg_water <- ground_estimates_forest_df$veg_water*.001
ground_estimates_forest_df <- ground_estimates_forest_df[c(1,2,5)]

#import VOD-> VWC raster and turn into data frame (repeated code)
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')
plot(vwc_from_vod )

#reample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_estimates_forest)

#merge with ground-based df
ground_estimates_forest_df <- merge(ground_estimates_forest_df,data.frame(rasterToPoints(vwc_from_vod)))

cor(ground_estimates_forest_df$veg_water,ground_estimates_forest_df$annual_storage_vwc_global_unfiltered)
0.87

ground_estimates_forest_df$veg <- 'Forest'

# compare to ground-based estimates in grasslands 


# Load in poa and herb X2 checked water contet, subset to grasslands
# units of water content are in gH2O/g dry mass

grassland_wc<-read.csv('./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')
head(grassland_wc)
#100 observations

grassland_wc <- grassland_wc %>%
  dplyr::filter(Exclude=='No')

#get to XYZ format
grassland_wc <-grassland_wc[c(2,3,4)]
summary(grassland_wc)
hist(grassland_wc$average.water.content)
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
  (ground_estimates_grassland_df$layer)*(ground_estimates_grassland_df$aboveground_dry_biomass_density_aggregate_30X)

#head(ground_estimates_grassland_df)

#convert to mm/m^2
ground_estimates_grassland_df$veg_water <- ground_estimates_grassland_df$veg_water*0.001
head(ground_estimates_grassland_df)

#import VOD-> VWC raster and turn into data frame (repeated code)
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#resample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_wc_raster)

#merge with ground-based df
ground_estimates_grassland_df <- merge(ground_estimates_grassland_df,
                             data.frame(rasterToPoints(vwc_from_vod)))


#merge with ground-based df
ground_estimates_grassland_df <- merge(ground_estimates_grassland_df,data.frame(rasterToPoints(vwc_from_vod)))



#dim(ground_estimates_grassland_df_filtered) #46 observations

cor(ground_estimates_grassland_df$annual_storage_vwc_global_unfiltered,
    ground_estimates_grassland_df$veg_water)
#0.90

#merge with others
ground_estimates_grassland_df <- ground_estimates_grassland_df[c(1,2,3,6)]
ground_estimates_grassland_df <- ground_estimates_grassland_df[c(1,2,4,3)]
ground_estimates_grassland_df$veg <- 'Grassland'

# compare to ground-based estimates in tundra


#import
ground_estimates <- read.csv('./../../../woodwater/Data/site_WC_estimates.csv')
#head(ground_estimates)

#use only cross-checked sites
ground_estimates <- subset(ground_estimates,Exclude=='No')
unique(ground_estimates$Exclude)

#narrow down/rename columns
ground_estimates <-ground_estimates[c('Land.Cover.Type', 'Lat','Long','mean.moisture')]
colnames(ground_estimates) <- c('Land.Cover.Type','y','x','moisture.content')

head(ground_estimates)

#turn into normally gridded raster
ground_estimates <-ground_estimates[c(1,3,2,4)]
ground_estimates <- na.exclude(ground_estimates)

#tundra
ground_estimates_tundra <- subset(ground_estimates,Land.Cover.Type=='Tundra')

ground_estimates_tundra <- fix_grid(ground_estimates_tundra[c(2,3,4)])
#plot(ground_estimates_tundra)

#re-import biomass data
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000

#since ground estimates are at coarser resolution, resample to that
dry_biomass_only <- resample(dry_biomass_only,ground_estimates_tundra)

#merge them
ground_estimates_tundra_df <-merge(data.frame(rasterToPoints(ground_estimates_tundra)),
                                   data.frame(rasterToPoints(dry_biomass_only)),
                                   by=c('x','y'))
#head(ground_estimates_tundra_df)
#length(unique(ground_estimates_df$layer)) 

#convert water content to water in g/m^2
ground_estimates_tundra_df$veg_water <- 
  ground_estimates_tundra_df$layer*ground_estimates_tundra_df$aboveground_dry_biomass_density_aggregate_30X

#convert from g/m^2 to mm/m^2
ground_estimates_tundra_df$veg_water <- ground_estimates_tundra_df$veg_water*.001
ground_estimates_tundra_df <- ground_estimates_tundra_df[c(1,2,5)]

#import VOD-> VWC raster and turn into data frame (repeated code)
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#reample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_estimates_tundra)

#merge with ground-based df
ground_estimates_tundra_df <- merge(ground_estimates_tundra_df,data.frame(rasterToPoints(vwc_from_vod)))

cor(ground_estimates_tundra_df$veg_water,ground_estimates_tundra_df$annual_storage_vwc_global_unfiltered)
0.73

ground_estimates_tundra_df$veg <- 'Tundra'

#combine with forest
ground_estimates_forest_tundra <- rbind(ground_estimates_forest_df,ground_estimates_tundra_df)

#combine with grassland
ground_estimates_forest_tundra_grassland <- rbind(ground_estimates_grassland_df,
                                                  ground_estimates_forest_tundra)

#try to plot it out
vwc_vod_plot <- ggplot(ground_estimates_forest_tundra_grassland,
       aes(veg_water,annual_storage_vwc_global_unfiltered,fill=veg)) +
  geom_point(size=5,pch=21) +
  scale_fill_manual(values=c('Grassland'='blue','Forest'='white',
                               'Tundra'='grey')) +
  #geom_smooth(method='lm',linetype='dashed') +
  annotate("text", x=9, y=8.3, label= "1:1 Line") +
  geom_abline(slope=1) +
  #geom_text(aes(label=x),hjust=0,vjust=0) +
  xlab('VOD-based water storage (mm)') +
  ylab('VWC-based water storage (mm)') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.25),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# estimate uncertainty: sum in quadrature ------


#first do it just for  VWC-based approach based on temporal variation

#relative or fractional basis
#grasslands_error <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_grasslands_quadrature_rel.tif')

#absolute basis (multiply fractional uncertainty by turnover)
global_error_raster <- raster('./../../../Data/Derived_Data/Uncertainty/quadrature/VWC_global_quadrature_rel.tif')
global_turnover_raster <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')

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





# estimate uncertainty: max-min approach-----

land_cover <-c('grasslands','forests','tundras','croplands','shrublands')
uncertainty.range.list<-list()

for(i in land_cover){
  
  min_raster <- 
    data.frame(rasterToPoints(raster(paste0("./../../../Data/Derived_Data/Uncertainty/min_max/VWC_",i,"_min.tif"))))
  colnames(min_raster) <- c('x','y','min')
  
  max_raster <- 
    data.frame(rasterToPoints(raster(paste0("./../../../Data/Derived_Data/Uncertainty/min_max/VWC_",i,"_max.tif"))))
  colnames(max_raster) <- c('x','y','max')
  
  merged <- merge(min_raster,max_raster,by=c('x','y'))
  
  merged$range <- merged$max - merged$min
  merged<- rasterFromXYZ(merged[c(1,2,5)])
  crs(merged) <- '+proj=longlat +datum=WGS84'
  
  uncertainty.range.list[[i]] <- merged
  
}

test.merge <- raster::merge(uncertainty.range.list$grasslands,
                            uncertainty.range.list$forests,
                            uncertainty.range.list$tundras,
                            uncertainty.range.list$croplands,
                            uncertainty.range.list$shrublands)
plot(log(test.merge))
summary(test.merge)

#transit time versus sensitivity -----


#annual transit map (A)
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')

global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                                    shrublands_unfiltered,tundras_unfiltered,
                                    croplands_unfiltered)
slopes.df <- read.csv('coefs.df.csv')
lc_names <- unique(slopes.df$region)

lc_list <- list()

for(i in lc_names){

sensitivity_raster <- subset(slopes.df,region==i)
sensitivity_raster <- rasterFromXYZ(sensitivity_raster[c(2,3,4)])
plot(sensitivity_raster)

sensitivity_raster_2 <- resample(sensitivity_raster,global_unfilitered)
global_unfilitered_2 <- crop(global_unfilitered,extent(sensitivity_raster_2))
global_unfilitered_2 <- mask(global_unfilitered_2,sensitivity_raster_2)
plot(global_unfilitered_2)

global_unfilitered_2_df <- data.frame(rasterToPoints(global_unfilitered_2))
sensitivity_raster_2_df <- data.frame(rasterToPoints(sensitivity_raster_2))

sens_transit <- merge(global_unfilitered_2_df,sensitivity_raster_2_df,by=c('x','y'))
sens_transit$lc < i

lc_list[[i]] <- sens_transit

}




plot(coef ~ layer, data=lc_list$shortgrass_steppe,xlab='transt time (days)',
     ylab='NPP sensitivity to annual rainfall',col='red')
plot(coef ~ layer, data=lc_list$northern_mixed_prairies,xlab='transt time (days)',
     ylab='NPP sensitivity to annual rainfall',col='blue')
plot(coef ~ layer, data=lc_list$hot_deserts,xlab='transt time (days)',
     ylab='NPP sensitivity to annual rainfall',col='maroon')
plot(coef ~ layer, data=lc_list$hot_deserts,xlab='transt time (days)',
     ylab='NPP sensitivity to annual rainfall',col='maroon')

summary(lm(slopes_raster ~ layer, data=sens_transit))



#minimum transit

grasslands_minimum <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_grassland_minimum_transit.tif')
forests_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_forest_minimum_transit.tif')
shrublands_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_shrubland_minimum_transit.tif')
tundras_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_tundra_minimum_transit.tif')
croplands_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_cropland_minimum_transit.tif')

global_minimum <- raster::merge(grasslands_minimum,forests_minimum,
                                shrublands_minimum,tundras_minimum,
                                croplands_minimum)





for(i in lc_names){
  
  sensitivity_raster <- subset(slopes.df,region==i)
  sensitivity_raster <- rasterFromXYZ(sensitivity_raster[c(2,3,4)])
  #plot(sensitivity_raster)
  
  sensitivity_raster_2 <- resample(sensitivity_raster,global_minimum)
  global_unfilitered_2 <- crop(global_minimum,extent(sensitivity_raster_2))
  global_unfilitered_2 <- mask(global_unfilitered_2,sensitivity_raster_2)
  plot(global_unfilitered_2)
  
  global_unfilitered_2_df <- data.frame(rasterToPoints(global_unfilitered_2))
  sensitivity_raster_2_df <- data.frame(rasterToPoints(sensitivity_raster_2))
  
  sens_transit <- merge(global_unfilitered_2_df,sensitivity_raster_2_df,by=c('x','y'))
  sens_transit$lc <- i
  
  lc_list[[i]] <- sens_transit
  
}


lc_df <- do.call('rbind',lc_list)

ggplot(lc_df,aes(layer,coef)) +
  facet_wrap(~lc,scales='free') +
  geom_point(aes(alpha=0.5),size=1) +
  #geom_smooth(method='lm')

sensitivity_raster_2 <- resample(sensitivity_raster,global_minimum)
global_unfilitered_2 <- crop(global_minimum,extent(sensitivity_raster_2))
global_unfilitered_2 <- mask(global_unfilitered_2,sensitivity_raster_2)
plot(global_unfilitered_2)

global_unfilitered_2_df <- data.frame(rasterToPoints(global_unfilitered_2))
sensitivity_raster_2_df <- data.frame(rasterToPoints(sensitivity_raster_2))

sens_transit <- merge(global_unfilitered_2_df,sensitivity_raster_2_df,by=c('x','y'))

plot(slopes_raster ~ layer, data=sens_transit,xlab='minimum transt time (days)',
     ylab='NPP sensitivity to annual rainfall')

summary(lm(slopes_raster ~ layer, data=sens_transit))

# violin plot ------

grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')

global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                                    shrublands_unfiltered,tundras_unfiltered,
                                    croplands_unfiltered)

unfiltered_data <- c(grasslands_unfiltered,forests_unfiltered,shrublands_unfiltered,
                     tundras_unfiltered,croplands_unfiltered)
annual_unfiltered_list_95<-list()

for(i in unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  quantile_transit_95 <- quantile(unfiltered_df$transit,prob=0.99)
  
  filtered_df = unfiltered_df %>%
    dplyr::filter(transit < quantile_transit_95)
  
  filtered_df$land_cover <- names(i)
  filtered_df$land_cover <- gsub("_unfiltered","",filtered_df$land_cover)
  filtered_df$land_cover <- gsub("annual_transit_vwc_","",filtered_df$land_cover)
  
  annual_unfiltered_list_95[[names(i)]] <- data.frame(filtered_df)
  
}

annual_filtered_df <- do.call('rbind',annual_unfiltered_list_95)
head(annual_filtered_df)
aggregate(transit~land_cover,max,data=annual_filtered_df)

?stat_function
cdf_annual_transit <- ggplot(annual_filtered_df,aes(x=land_cover,y=transit)) +
  geom_violin(width=3.1) +
  geom_boxplot(width=.1) +
  # geom_point(stat="ecdf",size=1) +
  # geom_line(stat="ecdf",size=1) +
  #geom_function(fun=pgamma,args=list(shape=2,scale=3)) +
  # scale_y_continuous(expand = c(0,0),limits = c(0,1.03)) +
  # scale_x_continuous(expand = c(0,0),limits = c(0,41)) +
  # scale_colour_manual(values=c('grassland'='blue','forest'='maroon',
  #                              'tundra'='grey','shrubland'='purple','cropland'='black'),
  #                     labels=c('grassland'='Grassland','forest'='Forest',
  #                              'tundra'='Tundra','shrubland'='Shrubland',
  #                              'cropland'='Cropland')) +
  # geom_hline(yintercept = 0.5,color='black',linetype='dashed') +
  # geom_hline(yintercept = 1,color='black',linetype='dashed') +
  ylab('Annual transit time (days)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.75,0.70),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
# correlate storage with T ------

#load annual storage
grasslands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_unfiltered_storge<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')


storage = raster::merge(grasslands_unfiltered_storge,forests_unfiltered_storge,
                        shrublands_unfiltered_storge,tundras_unfiltered_storge,
                        croplands_unfiltered_storge)

plot(storage)

storage_df <- data.frame(rasterToPoints(storage))
head(storage_df)

cor.list <- list()
slope.list <- list()
model_list <- list()
lc_names_2 <- c('Grassland','Shrubland','Cropland','Forest','Tundra')

for(i in lc_names_2){

#load annual cumulative T
grassland_annual_stack = import_cumulative_transp(i)
grassland_annual_stack <- resample(grassland_annual_stack,storage)

transp_df <- data.frame(rasterToPoints(grassland_annual_stack))

colnames(transp_df) <- c('x','y','canopy_transpiration_mm_m2')

# get rid of pixels where cumulative T is zero
transp_df <- transp_df %>%
  dplyr::filter(canopy_transpiration_mm_m2 > 1)
head(transp_df)

merge_df <- merge(transp_df,storage_df,by=c('x','y'))


cor.list[[i]] <- cor(merge_df$layer,merge_df$canopy_transpiration_mm_m2)
slope.list[[i]] <- coef(lm(canopy_transpiration_mm_m2~layer,merge_df))[2]
model_list[[i]] <- summary(lm(canopy_transpiration_mm_m2~layer,merge_df))
df_list[[i]] <- merge_df

# plot(canopy_transpiration_mm_m2~layer,data=merge_df, xlab='S',ylab='T',
#      main=i)

}


model1 <- lm(canopy_transpiration_mm_m2~layer,merge_df)
summary(model1)

# correlate T with storage for summer months -----



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

ecoregion_list <- c('Grassland','Forest','Shrubland','Tundra','Cropland')
summer.ecoregion.list <- list()
summer.slopes.list <- list()

for(i in ecoregion_list){

seasonal <- import_monthly_transp_2(i)

if(i=='Grassland'){
  
  ecoregion = 'grassland'}else if(i == 'Forest'){

  ecoegion = 'forest'}else if(i == 'Shrubland'){
  
  ecoregion = 'shrubland'}else if(i == 'Tundra'){
    
  ecoregion = 'tundra'}else if(i =='Cropland'){
    
  ecoregion = 'cropland'
  
  }


transp.stack = seasonal

test_turnover_function <- get_seasonal_turnover_2(season='june_august')

test_turnover_function$season <- 'summer'

test_turnover_function$ecoregion <- i

summer.slopes.list[[i]] <- coef(lm(canopy_transpiration_mm_m2~storage_mm,data=test_turnover_function))
summer.ecoregion.list[[i]] <- test_turnover_function

}


#bind to single data frame
summer.df<-do.call('rbind',summer.ecoregion.list)
head(summer.df)

#correlation across the globe
cor(summer.df$canopy_transpiration_mm_m2,summer.df$storage_mm)
#0.29

#by ecoregion


for(i in ecoregion_list){
  
  print(i)
  summer_subset <- subset(summer.df,ecoregion==i)
  print(cor(summer_subset$canopy_transpiration_mm_m2,summer_subset$storage_mm))
  
  
}




#compare transit with isotope estimates -----

#import annual transit
# grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
# forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
# shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
# tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
# croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')
# 
# global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
#                                     shrublands_unfiltered,tundras_unfiltered,
#                                     croplands_unfiltered)




#now break up by seasons of each measurement to get more precision
isotope <- read.csv('./../../../Data/Isotopes/isotope_data.csv')
head(isotope)
isotope <- subset(isotope,Drop.=='No')

unique(isotope$season)

# annual scale

grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')
#barren_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_barren_unfiltered.tif')
#plot(global_unfilitered)

global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                                    shrublands_unfiltered,tundras_unfiltered,
                                    croplands_unfiltered)

#list to store outputs
df_list_annual <- list()

#check this point
coord.check <- isotope[12,]
coord.check <- cbind(coord.check[4],coord.check[3])
raster::extract(global_unfilitered,coord.check,weights=TRUE,fun=mean,na.rm=T)
plot(global_unfilitered)
points(coord.check)


for(i in 1:nrow(isotope)){
  
  df <- isotope[i,]
  
  land_cover <- unique(df$Land.cover.type)
  
  coord <- cbind(df[4],df[3]) #get coordinate
  
  transit.isotope <- df[7]

  value <- raster::extract(global_unfilitered,coord)
    

  
  df.2 <- data.frame(coord)
  df.2$transit.vod <- value
  df.2$transit.isotope <- as.numeric(transit.isotope)
  df.2$cover <- land_cover
  
  df_list_annual[[i]] <-df.2
  
  
}

transit.df.annual.2 <- data.frame(do.call('rbind',df_list_annual))
transit.df.annual.2 <-na.omit(transit.df.annual.2)
plot(transit.vod~transit.isotope,data=transit.df.annual.2)
abline(a=0,b=1)
?abline
cor(transit.df.annual.2$transit.vod,transit.df.annual.2$transit.isotope)

#plot and save to file
vwc_isotope_plot <- ggplot(transit.df.annual.2,
                       aes(transit.isotope,transit.vod,fill=cover)) +
  geom_point(size=2,pch=21) +
  scale_fill_manual(values=c('Forest'='black','Shrublands'='white')) +
  #geom_smooth(method='lm',linetype='dashed') +
  annotate("text", x=7.6, y=5.8, label= "1:1 Line",size=3) +
  geom_abline(slope=1) +
  #geom_text(aes(label=x),hjust=0,vjust=0) +
  ylab('Satellite-based transit time (days)') +
  xlab('Isotope-based transit time (days)') +
  theme(
    axis.text.x = element_text(color='black',size=5), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=5),
    axis.title.x = element_text(color='black',size=6),
    axis.title.y = element_text(color='black',size=6),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=5),
    legend.position = c(0.7,0.3),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

png(height = 2000,width=2500,res=300,'Figures/october_2021/VOD_Isotope_TT_Comparison_2.png')
vwc_isotope_plot  
dev.off()





#check point
plot(Spring_global)
plot(global_unfilitered)


#do it by seasons 

season_list <- c('winter','spring','summer','fall')
ecoregion_list <- c('grassland','cropland','forest','shrubland','tundra')
winter_list = list()
spring_list = list()
summer_list = list()
fall_list = list()

for(i in ecoregion_list){
  
  #winter
  winter_raster <- raster(paste0('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_',i,'_unfiltered.tif'))
  winter_list[[i]] <- winter_raster
  
  #spring
  spring_raster <- raster(paste0('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_',i,'_unfiltered.tif'))
  spring_list[[i]] <- spring_raster
  
  #summer
  summer_raster <- raster(paste0('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_',i,'_unfiltered.tif'))
  summer_list[[i]] <- summer_raster
  
  #fall
  fall_raster <- raster(paste0('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_',i,'_unfiltered.tif'))
  fall_list[[i]] <- fall_raster
  
  
  
}


season_list <- c(winter_list,spring_list,summer_list,fall_list)
season_list <- c('winter_list','spring_list')

for(i in season_list){}
  

combine_land_cover <-  function(x){
  
  global_raster <- raster::merge(x$grassland,x$forest,x$cropland,x$shrubland,
                                 x$tundra)
  
  return(global_raster)
  
}

Winter_global <- combine_land_cover(winter_list)
Spring_global <- combine_land_cover(spring_list)
Summer_global <- combine_land_cover(summer_list)
Fall_global <- combine_land_cover(fall_list)

df_list = list()

for(i in 1:nrow(isotope)){
  
  df <- isotope[i,]
  
  coord <- cbind(df[4],df[3]) #get coordinate
  
  transit.isotope <- df[7]
  
  season <- df$season
  
  if(season=='Winter'){
    
    value <- extract(Winter_global,coord,weights=TRUE,fun=mean)
    
  }else if(season=='Spring'){
    
    value <- extract(Spring_global,coord,weights=TRUE,fun=mean)
    
    
  }else if(season=='Summer'){
    
    value <- extract(Summer_global,coord,weights=TRUE,fun=mean)
    
  }else if(season=='Fall'){
    
    value <- extract(Fall_global,coord,weights=TRUE,fun=mean)
    
    
  }
  


  df.2 <- data.frame(coord)
  df.2$transit.vod <- value
  df.2$transit.isotope <- as.numeric(transit.isotope)
  
  df_list[[i]] <-df.2
  
  
}

transit.df.2 <- data.frame(do.call('rbind',df_list))
na.omit(transit.df.2)

plot(transit.vod~transit.isotope,data=na.omit(transit.df.2))

transit.df.3 <- na.omit(transit.df.2)
cor(transit.df.3$transit.vod,transit.df.3$transit.isotope)
#0.79


summary(lm(transit.vod~transit.isotope,data=transit.df.3))


#compare storage and vod V 2 (looped) ------

#load in VWC data
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#load in ground estimates from literature
ground_estimates <- read.csv('./../../../woodwater/Data/site_WC_estimates.csv')
#head(ground_estimates)
unique(ground_estimates$Land.Cover.Type)

#use only cross-checked sites
ground_estimates <- subset(ground_estimates,Exclude=='No')
unique(ground_estimates$Exclude)

#narrow down/rename columns
ground_estimates <-ground_estimates[c('Land.Cover.Type', 'Lat','Long','mean.moisture')]
colnames(ground_estimates) <- c('Land.Cover.Type','y','x','moisture.content')

head(ground_estimates)


#load in aboveground biomass data
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000

#turn into normally gridded raster
ground_estimates <-ground_estimates[c(1,3,2,4)]
ground_estimates <- na.exclude(ground_estimates)

df.storage.list = list()
for(i in 1:nrow(ground_estimates)){

df <- ground_estimates[i,]
cover <- df$Land.Cover.Type

coord <- cbind(df[2],df[3]) #get coordinate

moisture.content <- as.numeric(df[4])


agb <- extract(dry_biomass_only,coord)

storage.ground <- (agb*moisture.content)*.001

df.2 <- data.frame(coord)
df.2$vwc.ground <- storage.ground
df.2$cover <- as.factor(cover)

df.storage.list[[i]] <- df.2


}

df.storage <- do.call('rbind',df.storage.list)


vod_vwc <- list() 
for(i in 1:nrow(df.storage)){
  
  
  df <- df.storage[i,]
  cover <- df$cover
  vwc.ground <- as.numeric(df$vwc.ground)
  
  coord <- cbind(df[1],df[2]) 
  
  
  vod.vwc <- extract(vwc_from_vod,coord)
  
  vod.vwc.df <- data.frame(coord)
  vod.vwc.df$vod.vwc <- as.numeric(vod.vwc)
  vod.vwc.df$vwc.ground <- as.numeric(vwc.ground)
  vod.vwc.df$cover <- cover
  
  vod_vwc[[i]] <- vod.vwc.df
  
  
}

vod_vwc_df <- do.call('rbind',vod_vwc)

vod_vwc_df <- na.exclude(vod_vwc_df)
plot(vod.vwc~vwc.ground,data=vod_vwc_df)
cor(vod_vwc_df$vod.vwc,vod_vwc_df$vwc.ground)
#0.7

#try to plot it out
vwc_vod_plot <- ggplot(vod_vwc_df,
                       aes(vwc.ground,vod.vwc,fill=cover)) +
  geom_smooth(data=vod_vwc_df,mapping=aes(vwc.ground,vod.vwc,group=2),fullrange=T,
              method='lm',linetype='dashed',color='black',se=F,legend=F) +
  geom_point(size=5,pch=21) +
  scale_fill_manual(values=c('Grassland'='blue','Forest'='white',
                             'Tundra'='grey','Shrubland'='green')) +
  annotate("text", x=9, y=9.7, label= "1:1 Line") +
  annotate("text", x=9.5, y=8, label= "Slope") +
  annotate("text", x=2.5, y=10, label= "r = 0.70",size=8) +
  geom_abline(slope=1) +
  #geom_text(aes(label=x),hjust=0,vjust=0) +
  ylab(bquote('VOD-based water storage'~(mm/m^2))) +
  xlab(bquote('VWC-based water storage'~(mm/m^2))) +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=19),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.15),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

png(height = 2000,width=2500,res=300,'Figures/october_2021/VOD_VWC_Storage_Comparison_2.png')
vwc_vod_plot 
dev.off()


cor(vod_vwc_df$vod.vwc,vod_vwc_df$vwc.ground)
summary(lm(vod.vwc~vwc.ground,data=vod_vwc_df))


#look at how storage and T covary through time in temperate and tropical latitudes-----


#step 1

#line up monthly transp and storage data

#this get you monthly estimates of storage:
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
#str(test.vwc)

#this will get you to a dataframe of monthly estimates of turnover, storage, and T.

#get monthly transpiration for forests
forest_minimum <- import_monthly_transp_2('Forest')
ecoregion = 'forest'
transp.stack = forest_minimum

#for grasslands
# grassland_minimum <- import_monthly_transp_2('Grassland')
# ecoregion = 'grassland'
# transp.stack = grassland_minimum

months<- c('january','february','march','april','may','june','july','august',
           'september','october','november','december')

months.list<-list()

for(i in months){
  
  test_turnover_function<-get_monthly_turnover_2(i)
  
  test_turnover_function$month <- i
  
  months.list[[i]] <- test_turnover_function
  
  
}

monthly.df <- do.call('rbind',months.list)
head(monthly.df)

monthly.df.no.inf <- monthly.df %>%
  filter(!turnover_days == 'Inf')

#filter to just temperate forests
temperate <-  monthly.df %>%
  filter(y > 25) %>%
  filter(y < 50)

#filter to the tropics
tropics <-  monthly.df %>%
  filter(y > -23) %>%
  filter(y < 23)


#take a look
plot(rasterFromXYZ(tropics[c(1,2,3)]))

month.coef.list<- list()
summary.list <- list()

#get spatial slopes for each month
for( i in months){
  
  northern.hem.subset <- subset(tropics,month==i)
  summary <- summary(lm(canopy_transpiration_mm_m2~storage_mm,data=northern.hem.subset))
  coef <- coef(lm(canopy_transpiration_mm_m2~storage_mm,data=northern.hem.subset))[2]
  coef <- data.frame(coef)
  coef$month <- i
  
  month.coef.list[[i]] <- coef
  summary.list[[i]] <- summary
  
  
}

#convert to dataframe
month.coef.list.df <- do.call('rbind',month.coef.list)

#I don't actually know what this tells us. But lets us this to convert months to numbers:
month.coef.list.df$month.2 <- seq.int(nrow(month.coef.list.df))
str(month.coef.list.df)

#merge this with original dataframe so months are numeric
temperate.2 <- merge(tropics,month.coef.list.df[c(2,3)],by='month')
temperate.2$canopy_transpiration_mm_m2_2 <- temperate.2$canopy_transpiration_mm_m2*30.5
plot(canopy_transpiration_mm_m2_2~month.2,data=temperate.2)
points(storage_mm~month.2,data=temperate.2,col='red')

forest<- ggplot(temperate.2,aes(month.2,canopy_transpiration_mm_m2_2)) +
  stat_summary(fun = 'mean',geom='point') +
  stat_summary(fun = 'mean',geom='line') +
  stat_summary(aes(month.2,storage_mm),
               fun='mean',col='red') +
  stat_summary(aes(month.2,storage_mm),
               fun='mean',col='red',geom='line')




