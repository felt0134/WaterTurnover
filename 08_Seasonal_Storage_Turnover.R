
#load in these scripts first
source('05_Import_Storage_Transp_Data.R')
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)

#load VWC data-----

outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")
ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016

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

#quantify pixel sample size (needs work) 
# test.vwc<-do.call('rbind',vwc.list)
# vwc.list.df <- aggregate(vwc~x+y,length,data=vwc.list.df)
# head(vwc.list.df)
# summary(test.vwc)
# vwc.list.df <- rasterFromXYZ(vwc.list.df)

#may have to load each mont in inuducally and re-grid it.

#do a loop for each season for each land cover type-----

# test.filter<- test.vwc %>%
#   dplyr::filter(month == c('12','1','2'))
# unique((test.filter$month))

seasons <- c('december_february','march_may','june_august','september_november')


#grasslands----
seasons.list.grassland<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='grassland')
  
  test_turnover_function$season <- i
  
  seasons.list.grassland[[i]] <- test_turnover_function
  
  
}

seasons.grasslands.df <- do.call('rbind',seasons.list.grassland)
rm(seasons.list.grassland)
#head(seasons.grasslands.df)
aggregate(turnover~season,median,data=seasons.grasslands.df)


#save the unfiltered files

#winter
grasslands_winter_unfiltered <- subset(seasons.grasslands.df,season=='december_february')
grasslands_winter_unfiltered <- rasterFromXYZ(grasslands_winter_unfiltered[c(1,2,5)])
crs(grasslands_winter_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(grasslands_winter_unfiltered)
writeRaster(grasslands_winter_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_grassland_unfiltered.tif')
rm(grasslands_winter_unfiltered)

#spring
grasslands_spring_unfiltered <- subset(seasons.grasslands.df,season=='march_may')
grasslands_spring_unfiltered <- rasterFromXYZ(grasslands_spring_unfiltered[c(1,2,5)])
crs(grasslands_spring_unfiltered) <- '+proj=longlat +datum=WGS84'
#plot(grasslands_spring_unfiltered)
writeRaster(grasslands_spring_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_grassland_unfiltered.tif')
rm(grasslands_spring_unfiltered)

#summer
grasslands_summer_unfiltered <- subset(seasons.grasslands.df,season=='june_august')
grasslands_summer_unfiltered <- rasterFromXYZ(grasslands_summer_unfiltered[c(1,2,5)])
crs(grasslands_summer_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(grasslands_summer_unfiltered)
writeRaster(grasslands_summer_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_grassland_unfiltered.tif')
rm(grasslands_summer_unfiltered)

#fall
grasslands_fall_unfiltered <- subset(seasons.grasslands.df,season=='september_november')
grasslands_fall_unfiltered <- rasterFromXYZ(grasslands_fall_unfiltered[c(1,2,5)])
crs(grasslands_fall_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(grasslands_fall_unfiltered)
writeRaster(grasslands_fall_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_grassland_unfiltered.tif')
rm(grasslands_fall_unfiltered)


#forests----
seasons.list.forest<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='forest')
  
  test_turnover_function$season <- i
  
  
  seasons.list.forest[[i]] <- test_turnover_function
  
  
}

seasons.forests.df <- do.call('rbind',seasons.list.forest)
rm(seasons.list.forest)
#head(seasons.forests.df)

#save the unfiltered files

#winter
forests_winter_unfiltered <- subset(seasons.forests.df,season=='december_february')
forests_winter_unfiltered <- rasterFromXYZ(forests_winter_unfiltered[c(1,2,5)])
crs(forests_winter_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(forests_winter_unfiltered)
writeRaster(forests_winter_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_forest_unfiltered.tif')
rm(forests_winter_unfiltered)

#spring
forests_spring_unfiltered <- subset(seasons.forests.df,season=='march_may')
forests_spring_unfiltered <- rasterFromXYZ(forests_spring_unfiltered[c(1,2,5)])
crs(forests_spring_unfiltered) <- '+proj=longlat +datum=WGS84'
#plot(forests_spring_unfiltered)
writeRaster(forests_spring_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_forest_unfiltered.tif')
rm(forests_spring_unfiltered)

#summer
forests_summer_unfiltered <- subset(seasons.forests.df,season=='june_august')
forests_summer_unfiltered <- rasterFromXYZ(forests_summer_unfiltered[c(1,2,5)])
crs(forests_summer_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(forests_summer_unfiltered)
writeRaster(forests_summer_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_forest_unfiltered.tif')
rm(forests_summer_unfiltered)

#fall
forests_fall_unfiltered <- subset(seasons.forests.df,season=='september_november')
forests_fall_unfiltered <- rasterFromXYZ(forests_fall_unfiltered[c(1,2,5)])
crs(forests_fall_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(forests_fall_unfiltered)
writeRaster(forests_fall_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_forest_unfiltered.tif')
rm(forests_fall_unfiltered)

#croplands-----
seasons.list.cropland<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='cropland')
  
  test_turnover_function$season <- i
  
  
  seasons.list.cropland[[i]] <- test_turnover_function
  
  
}

seasons.croplands.df <- do.call('rbind',seasons.list.cropland)
rm(seasons.list.cropland)
head(seasons.croplands.df)

#save the unfiltered files

#winter
croplands_winter_unfiltered <- subset(seasons.croplands.df,season=='december_february')
croplands_winter_unfiltered <- rasterFromXYZ(croplands_winter_unfiltered[c(1,2,5)])
crs(croplands_winter_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(croplands_winter_unfiltered)
writeRaster(croplands_winter_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_cropland_unfiltered.tif')
rm(croplands_winter_unfiltered)

#spring
croplands_spring_unfiltered <- subset(seasons.croplands.df,season=='march_may')
croplands_spring_unfiltered <- rasterFromXYZ(croplands_spring_unfiltered[c(1,2,5)])
crs(croplands_spring_unfiltered) <- '+proj=longlat +datum=WGS84'
#plot(croplands_spring_unfiltered)
writeRaster(croplands_spring_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_cropland_unfiltered.tif')
rm(croplands_spring_unfiltered)

#summer
croplands_summer_unfiltered <- subset(seasons.croplands.df,season=='june_august')
croplands_summer_unfiltered <- rasterFromXYZ(croplands_summer_unfiltered[c(1,2,5)])
crs(croplands_summer_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(croplands_summer_unfiltered)
writeRaster(croplands_summer_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_cropland_unfiltered.tif')
rm(croplands_summer_unfiltered)

#fall
croplands_fall_unfiltered <- subset(seasons.croplands.df,season=='september_november')
croplands_fall_unfiltered <- rasterFromXYZ(croplands_fall_unfiltered[c(1,2,5)])
crs(croplands_fall_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(croplands_fall_unfiltered)
writeRaster(croplands_fall_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_cropland_unfiltered.tif')
rm(croplands_fall_unfiltered)

#shrublands------
seasons.list.shrubland<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='shrubland')
  
  test_turnover_function$season <- i
  
  
  seasons.list.shrubland[[i]] <- test_turnover_function
  
  
}

seasons.shrublands.df <- do.call('rbind',seasons.list.shrubland)
rm(seasons.list.shrubland)
#head(seasons.shrublands.df)

#save the unfiltered files

#winter
shrublands_winter_unfiltered <- subset(seasons.shrublands.df,season=='december_february')
shrublands_winter_unfiltered <- rasterFromXYZ(shrublands_winter_unfiltered[c(1,2,5)])
crs(shrublands_winter_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(shrublands_winter_unfiltered)
writeRaster(shrublands_winter_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_shrubland_unfiltered.tif')
rm(shrublands_winter_unfiltered)

#spring
shrublands_spring_unfiltered <- subset(seasons.shrublands.df,season=='march_may')
shrublands_spring_unfiltered <- rasterFromXYZ(shrublands_spring_unfiltered[c(1,2,5)])
crs(shrublands_spring_unfiltered) <- '+proj=longlat +datum=WGS84'
#plot(shrublands_spring_unfiltered)
writeRaster(shrublands_spring_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_shrubland_unfiltered.tif')
rm(shrublands_spring_unfiltered)

#summer
shrublands_summer_unfiltered <- subset(seasons.shrublands.df,season=='june_august')
shrublands_summer_unfiltered <- rasterFromXYZ(shrublands_summer_unfiltered[c(1,2,5)])
crs(shrublands_summer_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(shrublands_summer_unfiltered)
writeRaster(shrublands_summer_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_shrubland_unfiltered.tif')
rm(shrublands_summer_unfiltered)

#fall
shrublands_fall_unfiltered <- subset(seasons.shrublands.df,season=='september_november')
shrublands_fall_unfiltered <- rasterFromXYZ(shrublands_fall_unfiltered[c(1,2,5)])
crs(shrublands_fall_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(shrublands_fall_unfiltered)
writeRaster(shrublands_fall_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_shrubland_unfiltered.tif')
rm(shrublands_fall_unfiltered)



#tundra----
seasons.list.tundra<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='tundra')
  
  test_turnover_function$season <- i
  
  
  seasons.list.tundra[[i]] <- test_turnover_function
  
  
}

seasons.tundras.df <- do.call('rbind',seasons.list.tundra)
rm(seasons.list.tundra)
#head(seasons.tundras.df)

#save the unfiltered files

#winter
tundras_winter_unfiltered <- subset(seasons.tundras.df,season=='december_february')
tundras_winter_unfiltered <- rasterFromXYZ(tundras_winter_unfiltered[c(1,2,5)])
crs(tundras_winter_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(tundras_winter_unfiltered)
writeRaster(tundras_winter_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_tundra_unfiltered.tif')
rm(tundras_winter_unfiltered)

#spring
tundras_spring_unfiltered <- subset(seasons.tundras.df,season=='march_may')
tundras_spring_unfiltered <- rasterFromXYZ(tundras_spring_unfiltered[c(1,2,5)])
crs(tundras_spring_unfiltered) <- '+proj=longlat +datum=WGS84'
#plot(tundras_spring_unfiltered)
writeRaster(tundras_spring_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_tundra_unfiltered.tif')
rm(tundras_spring_unfiltered)

#summer
tundras_summer_unfiltered <- subset(seasons.tundras.df,season=='june_august')
tundras_summer_unfiltered <- rasterFromXYZ(tundras_summer_unfiltered[c(1,2,5)])
crs(tundras_summer_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(tundras_summer_unfiltered)
writeRaster(tundras_summer_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_tundra_unfiltered.tif')
rm(tundras_summer_unfiltered)

#fall
tundras_fall_unfiltered <- subset(seasons.tundras.df,season=='september_november')
tundras_fall_unfiltered <- rasterFromXYZ(tundras_fall_unfiltered[c(1,2,5)])
crs(tundras_fall_unfiltered) <- '+proj=longlat +datum=WGS84'
#summary(tundras_fall_unfiltered)
writeRaster(tundras_fall_unfiltered,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_tundra_unfiltered.tif')
rm(tundras_fall_unfiltered)

#global-----


#import, combine, and save global winter raster
winter_grasslands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_grassland_unfiltered.tif')
winter_forests <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_forest_unfiltered.tif')
winter_shrublands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_shrubland_unfiltered.tif')
winter_tundras <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_tundra_unfiltered.tif')
winter_croplands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_cropland_unfiltered.tif')

winter_global <- raster::merge(winter_grasslands,winter_forests,winter_shrublands,
                               winter_tundras,winter_croplands)
summary(winter_global)
#median=12 days

writeRaster(winter_global,
            './../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_global_unfiltered.tif')


#import, combine, and save global spring raster
spring_grasslands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_grassland_unfiltered.tif')
spring_forests <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_forest_unfiltered.tif')
spring_shrublands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_shrubland_unfiltered.tif')
spring_tundras <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_tundra_unfiltered.tif')
spring_croplands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_cropland_unfiltered.tif')

spring_global <- raster::merge(spring_grasslands,spring_forests,spring_shrublands,
                               spring_tundras,spring_croplands)
summary(spring_global)
#median=8.5 days

writeRaster(spring_global,
            './../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_global_unfiltered.tif')

#import, combine, and save global summer raster
summer_grasslands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_grassland_unfiltered.tif')
summer_forests <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_forest_unfiltered.tif')
summer_shrublands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_shrubland_unfiltered.tif')
summer_tundras <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_tundra_unfiltered.tif')
summer_croplands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_cropland_unfiltered.tif')

summer_global <- raster::merge(summer_grasslands,summer_forests,summer_shrublands,
                               summer_tundras,summer_croplands)
summary(summer_global)
#median=3.5 days

writeRaster(summer_global,
            './../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_global_unfiltered.tif')


#import, combine, and save global fall raster
fall_grasslands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_grassland_unfiltered.tif')
fall_forests <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_forest_unfiltered.tif')
fall_shrublands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_shrubland_unfiltered.tif')
fall_tundras <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_tundra_unfiltered.tif')
fall_croplands <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_cropland_unfiltered.tif')

fall_global <- raster::merge(fall_grasslands,fall_forests,fall_shrublands,
                             fall_tundras,fall_croplands)
summary(fall_global)
#median=9.8 days

writeRaster(fall_global,
            './../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_global_unfiltered.tif')



# extra/junk to delete later....-----
plot(winter_global)

summary(seasons.forests.df)

rbind_seasons <- rbind(seasons.forests.df,seasons.croplands.df,seasons.grasslands.df,
                       seasons.shrublands.df,seasons.tundras.df)

aggregate(turnover~season,median,data=rbind_seasons)
#Winter months considerably higher median

#filter out extreme values for each season
season_list<-unique(rbind_seasons$season)
season_list_store<-list()

for(i in season_list){
  
  
  season_subset<-subset(rbind_seasons,season==i)
  season_subset<-filter_extremes_turnover_IQR(season_subset)
  season_list_store[[i]] <- season_subset
  
}

# library(plotrix)
# 
# gap.boxplot(season_list_store_df$turnover,gap=list(top=c(30,250),bottom=c(NA,NA),))
season_list_store_df<-do.call('rbind',season_list_store)
# head(season_list_store_df)
#summary(season_list_store_df)
# 
# summary(season_list_store_df)
# boxplot(turnover~season,data=season_list_store_df)

barplot_median<-aggregate(turnover~season,median,data=season_list_store_df)

png('Figures/2016_median_global_transit_season_VWC.png',width=8,height=6,units="in",res=400)
barplot(turnover~season,data=barplot_median,ylab='Median global transit time (days)',
        xlab='Season')
dev.off()

# mapping -----

#try ggplot mapping
library("rnaturalearth")
library("rnaturalearthdata")
library('scico')
library('ggplot2')
library('viridis')

summary(min_turnover_grasslands)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# png('Figures/2016_minimum_transit_grasslands_VWC.png',width=8,height=6,units="in",res=400)
# ggplot(min_turnover_grasslands,aes(x=x, y=y, color = turnover)) +
#   geom_tile() +
#   scale_color_viridis(option = "D")
#   scale_color_scico_d(alpha=1,begin=0,end=1,direction=1,
#                       palette = 'batlow',aesthetics = 'colour')
#   dev.off()

summary(rbind_min_transit)

png('Figures/2016_minimum_transit_map_VWC.png',width=8,height=6,units="in",res=400)

ggplot(data = world) + geom_sf() + geom_point(data= rbind_min_transit,aes(x=x, y=y, color = turnover),size=0.1) +
  scale_color_gradient2(midpoint = 3.5, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  #scale_color_scico_d(alpha=1,begin=1,end=531,direction=1,palette = 'batlow',aesthetics = 'colour')
  ggtitle('Minimum transit time (days)') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

# try four-panel seasonal map

unique(season_list_store_df$season)
jan_march_subset<-subset(season_list_store_df,season=='jan_march')
april_june_subset<-subset(season_list_store_df,season=='april_june')
july_september_subset<-subset(season_list_store_df,season=='july_september')
october_december_subset<-subset(season_list_store_df,season=='october_december')

library(cowplot)

#jan-march
jan_march_plot<-
  ggplot(data = world) + geom_sf() + geom_point(data= jan_march_subset,aes(x=x, y=y, color = turnover),size=0.1) +
  scale_color_gradient2(midpoint = 11.5, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  ggtitle('January-March') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#april-june
april_june_plot<-
  ggplot(data = world) + geom_sf() + geom_point(data= april_june_subset,aes(x=x, y=y, color = turnover),size=0.1) +
  scale_color_gradient2(midpoint = 11.5, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  ggtitle('April-June') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#july-september

july_september_plot<-
  ggplot(data = world) + geom_sf() + geom_point(data= july_september_subset,aes(x=x, y=y, color = turnover),size=0.1) +
  scale_color_gradient2(midpoint = 11.5, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  ggtitle('July-September') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#October-December
october_december_plot<-
  ggplot(data = world) + geom_sf() + geom_point(data= october_december_subset,aes(x=x, y=y, color = turnover),size=0.1) +
  scale_color_gradient2(midpoint = 11.5, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  ggtitle('October-December') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



png('Figures/2016_seasonal_transit.png',
    width=1500,height=700,res=150)

# pdf('Tables_Figures/coefficient_distributions_figure.pdf',
#     width=17.5,height=4)

print(plot_grid(jan_march_plot, april_june_plot,july_september_plot,
                october_december_plot, labels = c('A', 'B','C','D'),ncol = 2, nrow=2,
                rel_widths = c(1.5, 1.5,1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 18))

dev.off()

