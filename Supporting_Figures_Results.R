
#Supporting figures for results

# water storage by land cover type -----

grasslands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_cropland_unfiltered.tif')

annual_storage_data <- c(grasslands_annual_storage,forests_annual_storage,shrublands_annual_storage,
                     tundras_annual_storage,croplands_annual_storage)
annual_storage_list_95<-list()

for(i in annual_storage_data){
  
  annual_storage_df<-as.data.frame(rasterToPoints(i))
  #head(annual_storage_df)
  colnames(annual_storage_df) <- c('x','y','storage')
  
  quantile_storage_95 <- quantile(annual_storage_df$storage,prob=0.95)
  
  filtered_df <- annual_storage_df %>%
    dplyr::filter(storage < quantile_storage_95)
  
  #filtered_df = annual_storage_df 
  filtered_df$land_cover <- names(i)
  filtered_df$land_cover <- gsub("annual_storage_vwc_","",filtered_df$land_cover)
  filtered_df$land_cover <- gsub("_unfiltered","",filtered_df$land_cover)
  
  annual_storage_list_95[[names(i)]] <- data.frame(filtered_df)
  
}

annual_storage_filtered_df <- do.call('rbind',annual_storage_list_95)
head(annual_storage_filtered_df)
# summary(annual_storage_filtered_df) #median=4
# max(annual_storage_filtered_df$storage)

#make and save plot to file
boxplot_annual_storage <- ggplot(annual_storage_filtered_df,aes(x=land_cover,y=storage)) +
  geom_violin(width=1.3) +
  geom_boxplot(width=.1) +
  ylab('Aboveground water storage (mm)') +
  xlab('') +
  scale_x_discrete(labels=c("cropland" = "Cropland", "forest" = "Forest",
                            "grassland" = "Grassland",'shrubland'='Shrubland',
                            'tundra'='Tundra')) +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=20),
    axis.title.y = element_text(color='black',size=20),
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

png(height = 2000,width=2000,res=300,'Figures/october_2021/land_cover_storage_supporting.png')
boxplot_annual_storage
dev.off()

#-------------------------------------------------------------------------------
# water storage by land cover type table -------

grasslands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_cropland_unfiltered.tif')

annual_storage_data <- c(grasslands_annual_storage,forests_annual_storage,shrublands_annual_storage,
                         tundras_annual_storage,croplands_annual_storage)

annual_storage_unfiltered_list_median<-list()
annual_storage_unfiltered_list_95ci<-list()
annual_storage_unfiltered_list_5ci<-list()

for(i in annual_storage_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','storage')
  
  # get median
  annual_median<-data.frame(round(median(unfiltered_df$storage),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','vegetation')
  
  #get 95th quantile
  annual_95 <- data.frame(round(quantile(unfiltered_df$storage,prob=0.95),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','vegetation')
  
  #get 5th quantile
  annual_5 <- data.frame(round(quantile(unfiltered_df$storage,prob=0.05),2))
  annual_5$vegetation <- names(i)
  colnames(annual_5) <- c('ci5','vegetation')
  
  # merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
  # summary(merge_annual_median_95)
  
  annual_storage_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  annual_storage_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  annual_storage_unfiltered_list_5ci[[names(i)]] <- data.frame(annual_5)
  
}

#bind to df
annual_unfiltered_median_df <- do.call('rbind',annual_storage_unfiltered_list_median)
annual_unfiltered_95ci_df <- do.call('rbind',annual_storage_unfiltered_list_95ci)
annual_unfiltered_5ci_df <- do.call('rbind',annual_storage_unfiltered_list_5ci)

#merge both * '==' doesn't work, '=' does work.
merge_annual_median_95 <- merge(annual_unfiltered_median_df,annual_unfiltered_5ci_df,
                                by=c('vegetation'))
merge_annual_median_95 <- merge(merge_annual_median_95,annual_unfiltered_95ci_df,
                                by=c('vegetation'))

#cleanup
merge_annual_median_95$vegetation <- gsub('annual_storage_vwc_','',merge_annual_median_95$vegetation)
merge_annual_median_95$vegetation <- gsub('_unfiltered','',merge_annual_median_95$vegetation)

#save

write.csv(merge_annual_median_95,'Figures/october_2021/annual_storage_summary.csv')



-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# seasonal variation in water storage -----

#summer


#winter storage (A)

grasslands_winter_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_grassland_unfiltered.tif')
forest_winter_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_forest_unfiltered.tif')
tundra_winter_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_tundra_unfiltered.tif')
shurbland_winter_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_shrubland_unfiltered.tif')
cropland_winter_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/winter_storage_vwc_cropland_unfiltered.tif')

winter_storage <- raster::merge(grasslands_winter_storage,forest_winter_storage,
                                tundra_winter_storage,shurbland_winter_storage,
                                cropland_winter_storage)



winter_storage_df <- data.frame(rasterToPoints(winter_storage))
head(winter_storage_df)

quantile_winter_storage_50 <- quantile(winter_transit_df$layer,prob=0.5)

winter_storage_plot <- ggplot()
winter_storage_plot <- winter_storage_plot + geom_raster(data = winter_storage_df, aes(x = x, y = y, fill = layer))
winter_storage_plot <- winter_storage_plot + coord_equal()
winter_storage_plot <- winter_storage_plot + scale_fill_gradientn('Storage (mm)',
                                                                  colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                                                  #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                                                  #labels=c("Minimum",0.5,"Maximum"),
                                                                  values = rescale(c(min(winter_transit_df$layer),
                                                                                     10,
                                                                                     10 + 0.01,
                                                                                     max(winter_transit_df$layer)))) +
  xlab('') +
  ylab('') +
  ggtitle('Winter') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.1,0.35),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#spring storage (B)


grasslands_spring_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_grassland_unfiltered.tif')
forest_spring_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_forest_unfiltered.tif')
tundra_spring_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_tundra_unfiltered.tif')
shurbland_spring_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_shrubland_unfiltered.tif')
cropland_spring_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/spring_storage_vwc_cropland_unfiltered.tif')

spring_storage <- raster::merge(grasslands_spring_storage,forest_spring_storage,
                                tundra_spring_storage,shurbland_spring_storage,
                                cropland_spring_storage)



spring_storage_df <- data.frame(rasterToPoints(spring_storage))
head(spring_storage_df)

quantile_spring_storage_50 <- quantile(spring_transit_df$layer,prob=0.5)

spring_storage_plot <- ggplot()
spring_storage_plot <- spring_storage_plot + geom_raster(data = spring_storage_df, aes(x = x, y = y, fill = layer))
spring_storage_plot <- spring_storage_plot + coord_equal()
spring_storage_plot <- spring_storage_plot + scale_fill_gradientn('Storage (mm)',
                                                                  colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                                                  #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                                                  #labels=c("Minimum",0.5,"Maximum"),
                                                                  values = rescale(c(min(spring_transit_df$layer),
                                                                                     10,
                                                                                     10 + 0.01,
                                                                                     max(spring_transit_df$layer)))) +
  xlab('') +
  ylab('') +
  ggtitle('Spring') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#summer storage (C)


grasslands_summer_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_grassland_unfiltered.tif')
forest_summer_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_forest_unfiltered.tif')
tundra_summer_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_tundra_unfiltered.tif')
shurbland_summer_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_shrubland_unfiltered.tif')
cropland_summer_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/summer_storage_vwc_cropland_unfiltered.tif')

summer_storage <- raster::merge(grasslands_summer_storage,forest_summer_storage,
                                tundra_summer_storage,shurbland_summer_storage,
                                cropland_summer_storage)



summer_storage_df <- data.frame(rasterToPoints(summer_storage))
head(summer_storage_df)

quantile_summer_storage_50 <- quantile(summer_transit_df$layer,prob=0.5)

summer_storage_plot <- ggplot()
summer_storage_plot <- summer_storage_plot + geom_raster(data = summer_storage_df, aes(x = x, y = y, fill = layer))
summer_storage_plot <- summer_storage_plot + coord_equal()
summer_storage_plot <- summer_storage_plot + scale_fill_gradientn('Storage (mm)',
                                                                  colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                                                  #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                                                  #labels=c("Minimum",0.5,"Maximum"),
                                                                  values = rescale(c(min(summer_transit_df$layer),
                                                                                     10,
                                                                                     10 + 0.01,
                                                                                     max(summer_transit_df$layer)))) +
  xlab('') +
  ylab('') +
  ggtitle('Summer') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#fall storage (D)


grasslands_fall_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_grassland_unfiltered.tif')
forest_fall_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_forest_unfiltered.tif')
tundra_fall_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_tundra_unfiltered.tif')
shurbland_fall_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_shrubland_unfiltered.tif')
cropland_fall_storage <- 
  raster('./../../../Data/Derived_Data/VWC/Seasonal/fall_storage_vwc_cropland_unfiltered.tif')

fall_storage <- raster::merge(grasslands_fall_storage,forest_fall_storage,
                                tundra_fall_storage,shurbland_fall_storage,
                                cropland_fall_storage)



fall_storage_df <- data.frame(rasterToPoints(fall_storage))
head(fall_storage_df)

quantile_fall_storage_50 <- quantile(fall_transit_df$layer,prob=0.5)

fall_storage_plot <- ggplot()
fall_storage_plot <- fall_storage_plot + geom_raster(data = fall_storage_df, aes(x = x, y = y, fill = layer))
fall_storage_plot <- fall_storage_plot + coord_equal()
fall_storage_plot <- fall_storage_plot + scale_fill_gradientn('Storage (mm)',
                                                                  colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                                                  #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                                                  #labels=c("Minimum",0.5,"Maximum"),
                                                                  values = rescale(c(min(fall_transit_df$layer),
                                                                                     10,
                                                                                     10 + 0.01,
                                                                                     max(fall_transit_df$layer)))) +
  xlab('') +
  ylab('') +
  ggtitle('Fall') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#plot it
png(height = 2000,width=4500,res=300,'Figures/october_2021/Seasonal_storage.png')

ggarrange(winter_storage_plot, spring_storage_plot,summer_storage_plot,
          fall_storage_plot,
          ncol = 2,nrow=2)

dev.off()


#look at summary stats
winter_storage_df$season <- 'winter'
spring_storage_df$season <- 'spring'
summer_storage_df$season <- 'summer'
fall_storage_df$season <- 'fall'

seasonal_storage_df <- rbind(winter_storage_df,spring_storage_df,summer_storage_df,
                             fall_storage_df)
head(seasonal_storage_df)
aggregate(layer~season,median,data=seasonal_storage_df)

#-------------------------------------------------------------------------------
# annual transit time by land cover table -----

# annual turnover

#import raster
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')

#first do a global look
# global_annual_transit = raster::merge(grasslands_unfiltered,forests_unfiltered,shrublands_unfiltered,
#                                       tundras_unfiltered, croplands_unfiltered)
# global_annual_transit_df <- data.frame(rasterToPoints(global_annual_transit))
# head(global_annual_transit_df)
# 
# # get median
# annual_median<-data.frame(round(median(global_annual_transit_df$layer),2))
# annual_median$vegetation <- 'Gobal'
# colnames(annual_median) <- c('median','vegetation')
# 
# #get 95th quantile
# annual_95 <- data.frame(round(quantile(global_annual_transit_df$layer,prob=0.95),2))
# annual_95$vegetation <- 'Global'
# colnames(annual_95) <- c('ci95','vegetation')
# 
# #get 5th quantile
# annual_5 <- data.frame(round(quantile(global_annual_transit_df$layer,prob=0.05),2))
# annual_5$vegetation <- 'Global'
# colnames(annual_5) <- c('ci5','vegetation')
# 
# global_annual_transit <- merge(annual_median,annual_5,by=c('vegetation'))
# str(annual_median)


#get  summary stats for each land cover type
annual_median <- c(grasslands_unfiltered,forests_unfiltered,shrublands_unfiltered,
                     tundras_unfiltered,croplands_unfiltered)

annual_unfiltered_list_median<-list()
annual_unfiltered_list_95ci<-list()
annual_unfiltered_list_5ci<-list()

for(i in unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  # get median
  annual_median<-data.frame(round(median(unfiltered_df$transit),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','vegetation')
  
  #get 95th quantile
  annual_95 <- data.frame(round(quantile(unfiltered_df$transit,prob=0.95),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','vegetation')
  
  #get 5th quantile
  annual_5 <- data.frame(round(quantile(unfiltered_df$transit,prob=0.05),2))
  annual_5$vegetation <- names(i)
  colnames(annual_5) <- c('ci5','vegetation')
  
  # merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
  # summary(merge_annual_median_95)
  
  annual_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  annual_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  annual_unfiltered_list_5ci[[names(i)]] <- data.frame(annual_5)
  
}

#bind to df
annual_unfiltered_median_df <- do.call('rbind',annual_unfiltered_list_median)
annual_unfiltered_95ci_df <- do.call('rbind',annual_unfiltered_list_95ci)
annual_unfiltered_5ci_df <- do.call('rbind',annual_unfiltered_list_5ci)

#merge both * '==' doesn't work, '=' does work.
merge_annual_median_95 <- merge(annual_unfiltered_median_df,annual_unfiltered_5ci_df,
                                by=c('vegetation'))
merge_annual_median_95 <- merge(merge_annual_median_95,annual_unfiltered_95ci_df,
                                by=c('vegetation'))

#cleanup
merge_annual_median_95$vegetation <- gsub('annual_transit_vwc_','',merge_annual_median_95$vegetation)
merge_annual_median_95$vegetation <- gsub('_unfiltered','',merge_annual_median_95$vegetation)

#save

write.csv(merge_annual_median_95,'Figures/october_2021/annual_transit_summary.csv')


#-------------------------------------------------------------------------------
# minimum transit time by land cover table -----

# minimum turnover

#import raster
grasslands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_grassland_minimum_transit.tif')
forests_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_forest_minimum_transit.tif')
shrublands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_shrubland_minimum_transit.tif')
croplands_min <- raster( './../../../Data/Derived_Data/Turnover/Minimum/VWC_cropland_minimum_transit.tif')
tundras_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_tundra_minimum_transit.tif')

# global_min <- raster::merge(grasslands_min,forests_min,shrublands_min,croplands_min,
#                             tundras_min)
# 
# #do global first
# global_min_unfiltered_df<-as.data.frame(rasterToPoints(global_min))
# #head(unfiltered_df)
# colnames(global_min_unfiltered_df) <- c('x','y','transit')
# 
# # get median
# annual_median<-data.frame(round(median(global_min_unfiltered_df$transit),2))
# annual_median$vegetation <- 'Global'
# colnames(annual_median) <- c('median','vegetation')
# 
# #get 95th quantile
# annual_95 <- data.frame(round(quantile(global_min_unfiltered_df$transit,prob=0.95),2))
# annual_95$vegetation <- 'Global'
# colnames(annual_95) <- c('ci95','vegetation')
# 
# #get 5th quantile
# annual_5 <- data.frame(round(quantile(global_min_unfiltered_df$transit,prob=0.05),2))
# annual_5$vegetation <- 'Global'
# colnames(annual_5) <- c('ci5','vegetation')



#get  summary stats
min_unfiltered_data <- c(grasslands_min,forests_min,shrublands_min,
                         tundras_min,croplands_min)
min_unfiltered_list_median<-list()
min_unfiltered_list_95ci<-list()
min_unfiltered_list_5ci<-list()

for(i in min_unfiltered_data){
  
  min_unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(min_unfiltered_df) <- c('x','y','transit')
  
  # get median
  annual_median<-data.frame(round(median(min_unfiltered_df$transit),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','vegetation')
  
  #get 95th quantile
  annual_95 <- data.frame(round(quantile(min_unfiltered_df$transit,prob=0.95),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','vegetation')
  
  #get 5th quantile
  annual_5 <- data.frame(round(quantile(min_unfiltered_df$transit,prob=0.05),2))
  annual_5$vegetation <- names(i)
  colnames(annual_5) <- c('ci5','vegetation')
  
  # merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
  # summary(merge_annual_median_95)
  
  min_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  min_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  min_unfiltered_list_5ci[[names(i)]] <- data.frame(annual_5)
  
}

#bind to df
min_unfiltered_median_df <- do.call('rbind',min_unfiltered_list_median)
min_unfiltered_95ci_df <- do.call('rbind',min_unfiltered_list_95ci)
min_unfiltered_5ci_df <- do.call('rbind',min_unfiltered_list_5ci)

#merge both * '==' doesn't work, '=' does work.
merge_min_median_95 <- merge(min_unfiltered_median_df,min_unfiltered_5ci_df,
                                by=c('vegetation'))
merge_min_median_95 <- merge(merge_min_median_95,min_unfiltered_95ci_df,
                                by=c('vegetation'))

#cleanup
merge_min_median_95$vegetation <- gsub('annual_transit_vwc_','',merge_min_median_95$vegetation)
merge_min_median_95$vegetation <- gsub('_unfiltered','',merge_min_median_95$vegetation)

#save

write.csv(merge_min_median_95,'Figures/october_2021/minimum_transit_summary.csv')


#-------------------------------------------------------------------------------
#seasonal transit time table (in progress) -----


#winter
grasslands_winter <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_grassland_unfiltered.tif')
forests_winter <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_forest_unfiltered.tif')
shrublands_winter <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_shrubland_unfiltered.tif')
croplands_winter <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_cropland_unfiltered.tif')
tundras_winter <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_tundra_unfiltered.tif')

global_winter <- raster::merge(grasslands_winter,forests_winter,shrublands_winter,
                               croplands_winter,tundras_winter)

global_winter <- data.frame(rasterToPoints(global_winter))
global_winter$season <- 'winter'


#spring
grasslands_spring <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_grassland_unfiltered.tif')
forests_spring <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_forest_unfiltered.tif')
shrublands_spring <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_shrubland_unfiltered.tif')
croplands_spring <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_cropland_unfiltered.tif')
tundras_spring <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_tundra_unfiltered.tif')

global_spring <- raster::merge(grasslands_spring,forests_spring,shrublands_spring,
                               croplands_spring,tundras_spring)

global_spring <- data.frame(rasterToPoints(global_spring))
global_spring$season <- 'spring'


#summer
grasslands_summer <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_grassland_unfiltered.tif')
forests_summer <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_forest_unfiltered.tif')
shrublands_summer <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_shrubland_unfiltered.tif')
croplands_summer <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_cropland_unfiltered.tif')
tundras_summer <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_tundra_unfiltered.tif')

global_summer <- raster::merge(grasslands_summer,forests_summer,shrublands_summer,
                               croplands_summer,tundras_summer)

global_summer <- data.frame(rasterToPoints(global_summer))
global_summer$season <- 'summer'


#fall
grasslands_fall <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_grassland_unfiltered.tif')
forests_fall <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_forest_unfiltered.tif')
shrublands_fall <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_shrubland_unfiltered.tif')
croplands_fall <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_cropland_unfiltered.tif')
tundras_fall <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_tundra_unfiltered.tif')

global_fall <- raster::merge(grasslands_fall,forests_fall,shrublands_fall,
                               croplands_fall,tundras_fall)

global_fall <- data.frame(rasterToPoints(global_fall))
global_fall$season <- 'fall'

#list of files
seasonal_transit_data <- rbind(global_winter,global_spring,global_summer,global_fall)
head(seasonal_transit_data)
aggregate(layer~season,median,data=seasonal_transit_data)

# min_unfiltered_list_seasonal_transit_median<-list()
# min_unfiltered_list_seasonal_transit_95ci<-list()
# min_unfiltered_list_seasonal_transit_5ci<-list()
# 
# for(i in 1:length(seasonal_transit_data)){
#   
#   df <- seasonal_transit_data[i]
#   
#   min_unfiltered_df<-as.data.frame(rasterToPoints(df))
#   #head(unfiltered_df)
#   colnames(min_unfiltered_df) <- c('x','y','transit')
#   
#   # get median
#   annual_median<-data.frame(round(median(min_unfiltered_df$transit),2))
#   annual_median$vegetation <- names(i)
#   colnames(annual_median) <- c('median','season')
#   
#   #get 95th quantile
#   annual_95 <- data.frame(round(quantile(min_unfiltered_df$transit,prob=0.95),2))
#   annual_95$vegetation <- names(i)
#   colnames(annual_95) <- c('ci95','season')
#   
#   #get 5th quantile
#   annual_5 <- data.frame(round(quantile(min_unfiltered_df$transit,prob=0.05),2))
#   annual_5$vegetation <- names(i)
#   colnames(annual_5) <- c('ci5','season')
#   
#   # merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
#   # summary(merge_annual_median_95)
#   
#   min_unfiltered_list_seasonal_transit_median[[names(i)]] <- data.frame(annual_median)
#   min_unfiltered_list_seasonal_transit_95ci[[names(i)]] <- data.frame(annual_95)
#   min_unfiltered_list_seasonal_transit_5ci[[names(i)]] <- data.frame(annual_5)
#   
# }
# 
# #bind to df
# min_unfiltered_median_df <- do.call('rbind',min_unfiltered_list_seasonal_transit_median)
# min_unfiltered_95ci_df <- do.call('rbind',min_unfiltered_list_seasonal_transit_95ci)
# min_unfiltered_5ci_df <- do.call('rbind',min_unfiltered_list_seasonal_transit_5ci)
# 
# #merge both * '==' doesn't work, '=' does work.
# merge_min_median_95 <- merge(min_unfiltered_median_df,min_unfiltered_5ci_df,
#                              by=c('vegetation'))
# merge_min_median_95 <- merge(merge_min_median_95,min_unfiltered_95ci_df,
#                              by=c('vegetation'))
# 
# #cleanup
# merge_min_median_95$vegetation <- gsub('annual_transit_vwc_','',merge_min_median_95$vegetation)
# merge_min_median_95$vegetation <- gsub('_unfiltered','',merge_min_median_95$vegetation)

#save

write.csv(merge_min_median_95,'Figures/october_2021/minimum_transit_summary.csv')



#-------------------------------------------------------------------------------
# summarize model coefficients: annual -------

get_95 <- function(x){
  
  quantile(x,probs=0.95)
  
}

get_5 <- function(x){
  
  quantile(x,probs=0.05)
  
}

get_50 <- function(x){
  
  quantile(x,probs=0.50)
  
}


#loop
climate_val <- c('aridity','ppt','pet')
coef_list <- list()

for(i in climate_val){
 
coef <- read.csv(paste0('./../../../Data/Derived_data/Model_outputs/annual_coef_',i,'.csv'))
head(coef)

coef_50 <- aggregate(climate_mean~veg,get_50,data=coef)
colnames(coef_50) <- c('veg','median')

coef_05 <- aggregate(climate_mean~veg,get_5,data=coef)
colnames(coef_05) <- c('veg','5th')

coef_95 <- aggregate(climate_mean~veg,get_95,data=coef)
colnames(coef_95) <- c('veg','95th')

coefs_ci <- merge(coef_50,coef_05,by='veg')
coefs_ci <- merge(coefs_ci,coef_95,by='veg')

coefs_ci$coef <- i


coef_list[[i]] <- coefs_ci



}

#
coefficients <- data.frame(do.call('rbind',coef_list))
write.csv(coefficients,'Figures/october_2021/coefficients_annual_transit_summaries.csv')


#-------------------------------------------------------------------------------
#summarize model coefficients: annual -------
  
  get_95 <- function(x){
    
    quantile(x,probs=0.95)
    
  }

get_5 <- function(x){
  
  quantile(x,probs=0.05)
  
}

get_50 <- function(x){
  
  quantile(x,probs=0.50)
  
}


#loop
climate_val <- c('aridity','ppt','pet')
coef_list <- list()

for(i in climate_val){
  
  coef <- read.csv(paste0('./../../../Data/Derived_data/Model_outputs/minimum_coef_',i,'.csv'))
  head(coef)
  
  coef_50 <- aggregate(climate_mean~veg,get_50,data=coef)
  colnames(coef_50) <- c('veg','median')
  
  coef_05 <- aggregate(climate_mean~veg,get_5,data=coef)
  colnames(coef_05) <- c('veg','5th')
  
  coef_95 <- aggregate(climate_mean~veg,get_95,data=coef)
  colnames(coef_95) <- c('veg','95th')
  
  coefs_ci <- merge(coef_50,coef_05,by='veg')
  coefs_ci <- merge(coefs_ci,coef_95,by='veg')
  
  coefs_ci$coef <- i
  
  
  coef_list[[i]] <- coefs_ci
  
  
  
}

#
coefficients <- data.frame(do.call('rbind',coef_list))
write.csv(coefficients,'Figures/october_2021/coefficients_minimum_transit_summaries.csv')


#-------------------------------------------------------------------------------
#correlation between average/range of storage and annual biomass ------

#import biomass
aboveground_biomass <- raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#import annual average storage
grasslands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_annual_storage <- raster('./../../../Data/Derived_Data/VWC/annual/annual_storage_vwc_cropland_unfiltered.tif')

annual_storage_global <- raster::merge(grasslands_annual_storage,forests_annual_storage,shrublands_annual_storage,
                         tundras_annual_storage,croplands_annual_storage)

#match grids and merge
annual_storage_global <- resample(annual_storage_global,aboveground_biomass)
annual_storage_biomass <- stack(annual_storage_global,aboveground_biomass)
annual_storage_biomass_df <- merge(data.frame(rasterToPoints(annual_storage_global)),
                                   data.frame(rasterToPoints(aboveground_biomass)))
#get cor
cor(annual_storage_biomass_df$layer,annual_storage_biomass_df$aboveground_dry_biomass_density_aggregate_30X)
#0.76

#import storage range
#import annual average storage
grasslands_range_storage <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_grassland_range_storage.tif')
forests_range_storage <- raster('./../../../Data/Derived_Data/VWC/range/VWC_forest_range_storage.tif')
shrublands_range_storage <- raster('./../../../Data/Derived_Data/VWC/range/VWC_shrubland_range_storage.tif')
tundras_range_storage <- raster('./../../../Data/Derived_Data/VWC/range/VWC_tundra_range_storage.tif')
croplands_range_storage <- raster('./../../../Data/Derived_Data/VWC/range/VWC_cropland_range_storage.tif')

range_storage_global <- raster::merge(grasslands_range_storage,forests_range_storage,shrublands_range_storage,
                                       tundras_range_storage,croplands_range_storage)

#match grids and merge
range_storage_global <- resample(range_storage_global,aboveground_biomass)
range_storage_biomass <- stack(range_storage_global,aboveground_biomass)
range_storage_biomass_df <- merge(data.frame(rasterToPoints(range_storage_global)),
                                   data.frame(rasterToPoints(aboveground_biomass)))
#get cor
cor(range_storage_biomass_df$layer,range_storage_biomass_df$aboveground_dry_biomass_density_aggregate_30X)


#-------------------------------------------------------------------------------
# minimum transit time model outputs -----


climate_list <-c('aridity','pet','ppt')
climate_list_minimum_df <- list()

for(i in climate_list){
  
  minimum_climate_coef <- 
    read.csv(paste0('./../../../Data/Derived_data/Model_outputs/minimum_coef_',i,'.csv')) 
  
  climate_list_minimum_df[[i]] <- minimum_climate_coef
  
  # min_climate_coef <- 
  #   read.csv.csv(paste0('./../../../Data/Derived_data/Model_outputs/minimum_coef_',i,'.csv')) 
  # 
  # climate_list_min_df[[i]] <- min_climate_coef
  
}


#minimum transit

#C
aridity_minimum <- ggplot(climate_list_minimum_df$aridity,aes(x=climate_mean,fill=veg)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.5,aes(y=..scaled..)) +
  scale_fill_manual(values=c('grassland'='blue','forest'='maroon',
                             'tundra'='grey','shrubland'='gold','cropland'='black'),
                    labels=c('grassland'='Grassland','forest'='Forest',
                             'tundra'='Tundra','shrubland'='Shrubland','cropland'='Cropland')) +
  xlab('Effect of MAP/PET') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.15,0.5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#B
pet_minimum <- ggplot(climate_list_minimum_df$pet,aes(x=climate_mean,fill=veg)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-.6,0.5)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('grassland'='blue','forest'='maroon',
                             'tundra'='grey','shrubland'='gold','cropland'='black'),
                    labels=c('grassland'='Grassland','forest'='Forest',
                             'tundra'='Tundra','shrubland'='Shrubland','cropland'='Cropland')) +
  xlab('Effect of PET on minimum transit time') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#C
map_minimum <- ggplot(climate_list_minimum_df$ppt,aes(x=climate_mean,fill=veg)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-.0011,0.0002)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('grassland'='blue','forest'='maroon',
                             'tundra'='grey','shrubland'='gold','cropland'='black'),
                    labels=c('grassland'='Grassland','forest'='Forest',
                             'tundra'='Tundra','shrubland'='Shrubland','cropland'='Cropland')) +
  xlab('Effect of MAP on minimum transit time') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


library(cowplot)
png(height = 1500,width=5500,res=300,'Figures/october_2021/Figure_X_coefs_minimum.png')


# png('Tables_Figures/coefficient_distributions_figure.png',
#     width=2550,height=700,res=150)
# 
# pdf('Figures/coefficient_distributions_figure.pdf',
#     width=17.5,height=4)

print(plot_grid(map_minimum, pet_minimum,aridity_minimum,labels = c('A', 'B','C'),ncol = 3, nrow=1,
                rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 18))

dev.off()

