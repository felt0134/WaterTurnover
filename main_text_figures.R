# figures version 2
# library(ggpubr)
# library(scales)
library(scico)
library(cowplot)

#figure 1: Storage ------

#mean annual storage (A)
#111*.083
grasslands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_unfiltered_storge<- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_unfiltered_storge <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')

storage = raster::merge(grasslands_unfiltered_storge,forests_unfiltered_storge,
                        shrublands_unfiltered_storge,tundras_unfiltered_storge,
                        croplands_unfiltered_storge)

Albers <-
  crs(
    '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  )

crs(storage) <- Albers
plot(storage)

#filter out zeros and truncate by 95th quantile
annual_storage <- data.frame(rasterToPoints(storage)) %>%
  filter(layer > 0)

#median(annual_storage$layer)
#4.26

annual_storage_truncate <- truncate_for_mapping(annual_storage,3)

#head(annual_storage)

storage_1 <- ggplot(annual_storage_truncate, aes(x = x, y = y, fill = value)) + 
  geom_raster() + 
  coord_equal() +
  #geom_sf()
  scale_fill_scico('Aboveground water storage (mm)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.12,0.3),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#figure out a way to get rid of the white space

#compare to other pools/estimates

pools <- read.csv('./../../../Data/Pools/H2OPoolSizeEstimates.csv')
head(pools)
pools$size <- as.numeric(as.character(pools$Size..km3.))
unique(pools$Pool)

#pool means
pool_means <- aggregate(Size..km3. ~ Pool,mean,data=pools)
(16500.000 - 1135.708)/1135.708

vegetation_pools <- subset(pools,Pool=='Vegetation')
barplot(size~Citation,data=vegetation_pools)

mean(vegetation_pools$size)

#pool_size <- ggplot(vegetation_pools, aes(x = size, y = reorder(Citation,size))) + 
pool_size <- ggplot(pools, aes(y = reorder(Pool,size), x = log(size)))  +
#geom_vline(xintercept = 1135.71) +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  scale_x_continuous(expand=c(0,0)) +
  #geom_errorbar(ymin=min(log(size),ymax=max(log(size)))) +
  geom_errorbar(data=vegetation_pools,mapping=aes(y=Pool,x=log(size)),
                xmin=5,xmax=10,size=0.5,width=.25) +
  #geom_point() + 
  ylab('') +
  #annotate("text", x=1450, y=2, label= "Average across studies") +
  xlab(bquote('Freshwater pool size'~(log(km^3)))) +
  theme(
    axis.text.x = element_text(color='black',size=13), #, angle=25,hjust=1),
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



#add with bivariate VOD VWC relationship
#https://wilkelab.org/cowplot/articles/plot_grid.html

plots <- align_plots(storage_1, pool_size, align = 'v', axis = 'l')
# then build the bottom row
bottom_row <- plot_grid(plots[[2]], vwc_vod_plot, labels = c('B', 'C'), label_size = 15)


png(height = 3500,width=4500,res=300,'Figures/october_2021/Figure_1_Storage_V2.png')

# then combine with the top row for final plot
plot_grid(plots[[1]], bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)

dev.off()


#-------------------------------------------------------------------------------
#figure 2: Transit ------


#https://rdrr.io/cran/scico/man/ggplot2-scales.html

#annual transit map (A)
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')

global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                                    shrublands_unfiltered,tundras_unfiltered,
                                    croplands_unfiltered)

#filter out zeros and truncate by 95th quantile
annual_transit <- data.frame(rasterToPoints(global_unfilitered)) %>%
  filter(layer > 0)

median(annual_transit$layer)

annual_transit_truncate <- truncate_for_mapping(annual_transit,3)
#head(annual_transit)
summary(annual_transit)

# Annual transit time map (A)
transit_1 <- ggplot(annual_transit_truncate, aes(x = x, y = y, fill = value)) + 
  geom_tile() + 
  scale_fill_scico('Annual transit time (days)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.12,0.3),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#latitude and annual transit time 

#average by latitude
lat <- aggregate(value~y,mean,data=annual_transit)
#head(lat)

transit_lat <- ggplot(lat,aes(value,y,color=value)) +
  scale_color_scico('Annual transit time (days)',palette = 'batlow',direction=-1) +
  geom_point(size=1) +
  xlab('Annual transit time (days)') +
  ylab('Latitude (degrees)') +
  geom_hline(yintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    #legend.position = c(0.6,0.25),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#annual transit by land cover type 
unfiltered_data <- c(grasslands_unfiltered,forests_unfiltered,shrublands_unfiltered,
                     tundras_unfiltered,croplands_unfiltered)
annual_unfiltered_list_95<-list()

for(i in unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  quantile_transit_95 <- quantile(unfiltered_df$transit,prob=0.95)
  
  filtered_df = unfiltered_df %>%
    dplyr::filter(transit < quantile_transit_95)
  
  filtered_df$land_cover <- names(i)
  filtered_df$land_cover <- gsub("_unfiltered","",filtered_df$land_cover)
  filtered_df$land_cover <- gsub("annual_transit_vwc_","",filtered_df$land_cover)
  
  annual_unfiltered_list_95[[names(i)]] <- data.frame(filtered_df)
  
}

annual_filtered_df <- do.call('rbind',annual_unfiltered_list_95)
summary(annual_filtered_df)
#head(annual_filtered_df)
#aggregate(transit~land_cover,max,data=annual_filtered_df)

boxplot_annual_transit <- ggplot(annual_filtered_df,aes(x=land_cover,y=transit,color=transit)) +
  scale_color_scico('Transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1.3) +
  geom_boxplot(width=.1) +
  ylab('Annual transit time (days)') +
  xlab('') +
  scale_x_discrete(labels=c("cropland" = "Cropland", "forest" = "Forest",
                            "grassland" = "Grassland",'shrubland'='Shrubland',
                            'tundra'='Tundra')) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.14,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



# #make the inset
# library(cowplot)
# plot.with.inset <-
#   ggdraw() +
#   draw_plot(boxplot_annual_transit) +
#   draw_plot(vwc_isotope_plot, x = .1, y = 0.60, width = .45, height = .40)
# 
# 
# #minimum transit time 
# 
# #load and combine land cover rasters
grasslands_minimum <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_grassland_minimum_transit.tif')
forests_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_forest_minimum_transit.tif')
shrublands_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_shrubland_minimum_transit.tif')
tundras_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_tundra_minimum_transit.tif')
croplands_minimum  <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_cropland_minimum_transit.tif')

global_minimum <- raster::merge(grasslands_minimum,forests_minimum,
                                shrublands_minimum,tundras_minimum,
                                croplands_minimum)

#filter out zeros and truncate by 95th quantile
minimum_transit <- data.frame(rasterToPoints(global_minimum)) %>%
  filter(layer > 0)

minimum_transit <- truncate_for_mapping(minimum_transit,3)
# #head(minimum_transit)

# Minimum transit time map 
transit_2 <- ggplot(minimum_transit, aes(x = x, y = y, fill = value)) + 
  geom_tile() + 
  scale_fill_scico('Minimum transit time (days)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.12,0.3),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#latitude and minimum transit time

#average minimum transit by latitude
lat_2 <- aggregate(value~y,mean,data=minimum_transit)

min_transit_lat <- ggplot(lat_2,aes(value,y,color=value)) +
  scale_color_scico('Annual transit time (days)',palette = 'batlow',direction=-1) +
  geom_point(size=1) +
  xlab('Minimum transit time (days)') +
  ylab('Latitude (degrees)') +
  geom_hline(yintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    #legend.position = c(0.6,0.25),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#minimum transit by land cover type 
unfiltered_data <- c(grasslands_minimum,forests_minimum,shrublands_minimum,
                     tundras_minimum,croplands_minimum)
min_unfiltered_list_95<-list()

for(i in unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  quantile_transit_95 <- quantile(unfiltered_df$transit,prob=0.95)
  
  filtered_df = unfiltered_df %>%
    dplyr::filter(transit < quantile_transit_95) %>%
    dplyr::filter(transit > 0) # remove any values below zero
  
  filtered_df$land_cover <- names(i)
  filtered_df$land_cover <- gsub("VWC_","",filtered_df$land_cover)
  filtered_df$land_cover <- gsub("_minimum_transit","",filtered_df$land_cover)
  
  min_unfiltered_list_95[[names(i)]] <- data.frame(filtered_df)
  
}

min_filtered_df <- do.call('rbind',min_unfiltered_list_95)
head(min_filtered_df)
aggregate(transit~land_cover,max,data=min_filtered_df)

boxplot_minimum_transit <- ggplot(min_filtered_df,aes(x=land_cover,y=transit,color=transit)) +
  scale_color_scico('Transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1.2) +
  geom_boxplot(width=.1) +
  ylab('Minimum transit time (days)') +
  xlab('') +
  scale_x_discrete(labels=c("cropland" = "Cropland", "forest" = "Forest",
                            "grassland" = "Grassland",'shrubland'='Shrubland',
                            'tundra'='Tundra')) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    #legend.title = element_blank(),
    legend.text = element_text(size=8),
    legend.position = c(0.14,0.8),
    legend.margin=margin(c(0.1,.1,.1,.1)),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'top',
    legend.key.size = unit(.50, 'cm'),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#create multipanel figure for manuscript

png(height = 2500,width=4500,res=300,'Figures/october_2021/Figure_2_transit.png')

print(plot_grid(transit_1, transit_lat,boxplot_annual_transit,
                transit_2,min_transit_lat,boxplot_minimum_transit,
                labels = c('A', 'B','C','D','E','F','G'),ncol = 3, nrow=2,
                rel_widths = c(2.5,1,2,2,2,2), 
                rel_heights = c(1,1,1,1,1,1),label_size = 15))

dev.off()

#create separate annual and minimum figures for presentations

# #annual transit
# png(height = 1500,width=4500,res=300,'./../../../Meetings/AGU 2021/Talk/annual_transit.png')
# 
# print(plot_grid(transit_1, transit_lat,boxplot_annual_transit,
#                 labels = c('A', 'B','C'),ncol = 3, nrow=1,
#                 rel_widths = c(2.75,1,2), 
#                 rel_heights = c(1,1,1),label_size = 15))
# 
# dev.off()
# 
# #minimum transit time
# png(height = 1500,width=4500,res=300,'./../../../Meetings/AGU 2021/Talk/minimum_transit.png')
# 
# print(plot_grid(transit_2,min_transit_lat,boxplot_minimum_transit,
#                 labels = c('A', 'B','C'),ncol = 3, nrow=1,
#                 rel_widths = c(2.75,1,2), 
#                 rel_heights = c(1,1,1),label_size = 15))
# 
# dev.off()



#-------------------------------------------------------------------------------
#figure 3: seasonal transit (NEEDS WORK) -------


#winter transit (A)

grasslands_winter_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_grassland_unfiltered.tif')
forest_winter_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_forest_unfiltered.tif')
tundra_winter_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_tundra_unfiltered.tif')
shurbland_winter_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_shrubland_unfiltered.tif')
cropland_winter_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_cropland_unfiltered.tif')

winter_transit <- raster::merge(grasslands_winter_transit,forest_winter_transit,
                                tundra_winter_transit,shurbland_winter_transit,
                                cropland_winter_transit)



winter_transit_df <- data.frame(rasterToPoints(winter_transit))
head(winter_transit_df)

quantile_winter_transit_50 <- quantile(winter_transit_df$layer,prob=0.5)

#quantile_winter_transit_95 <- quantile(winter_transit_df$layer,prob=0.90)
quantile_winter_transit_95 <- 100

winter_transit_df <- winter_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)

winter_transit_plot <- ggplot()
winter_transit_plot <- winter_transit_plot + geom_raster(data = winter_transit_df, aes(x = x, y = y, fill = layer))
winter_transit_plot <- winter_transit_plot + coord_equal()
winter_transit_plot <- winter_transit_plot + scale_fill_gradientn('Transit (days)',
                              colours=c("red", "yellow","white", "skyblue", "darkblue"),
                              #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                              #labels=c("Minimum",0.5,"Maximum"),
                              values = rescale(c(min(winter_transit_df$layer),
                                                 quantile_winter_transit_50,
                                                 quantile_winter_transit_50 + 0.01,
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




#spring transit (B)

grasslands_spring_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_grassland_unfiltered.tif')
forest_spring_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_forest_unfiltered.tif')
tundra_spring_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_tundra_unfiltered.tif')
shurbland_spring_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_shrubland_unfiltered.tif')
cropland_spring_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_cropland_unfiltered.tif')

spring_transit <- raster::merge(grasslands_spring_transit,forest_spring_transit,
                                tundra_spring_transit,shurbland_spring_transit,
                                cropland_spring_transit)



spring_transit_df <- data.frame(rasterToPoints(spring_transit))
head(spring_transit_df)

# quantile_spring_transit_50 <- quantile(spring_transit_df$layer,prob=0.5)
# 
# quantile_spring_transit_95 <- quantile(spring_transit_df$layer,prob=0.95)

spring_transit_df <- spring_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)

spring_transit_plot <- ggplot()
spring_transit_plot <- spring_transit_plot + 
  geom_raster(data = spring_transit_df, aes(x = x, y = y, fill = layer))
spring_transit_plot <- spring_transit_plot + coord_equal()
spring_transit_plot <- spring_transit_plot + 
  scale_fill_gradientn('',
                       colours=c("red", "yellow","white", "skyblue", "darkblue"),
                       #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                       #labels=c("Minimum",0.5,"Maximum"),
                       values = rescale(c(min(spring_transit_df$layer),
                                          quantile_winter_transit_50,
                                          quantile_winter_transit_50 + 0.01,
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
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5),
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




#summer transit (C)

grasslands_summer_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_grassland_unfiltered.tif')
forest_summer_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_forest_unfiltered.tif')
tundra_summer_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_tundra_unfiltered.tif')
shurbland_summer_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_shrubland_unfiltered.tif')
cropland_summer_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_cropland_unfiltered.tif')

summer_transit <- raster::merge(grasslands_summer_transit,forest_summer_transit,
                                tundra_summer_transit,shurbland_summer_transit,
                                cropland_summer_transit)



summer_transit_df <- data.frame(rasterToPoints(summer_transit))
head(summer_transit_df)

# quantile_summer_transit_50 <- quantile(summer_transit_df$layer,prob=0.5)
# 
# quantile_summer_transit_95 <- quantile(summer_transit_df$layer,prob=0.95)

summer_transit_df <- summer_transit_df %>%
  filter(layer > 0) 

median(summer_transit_df$layer)
#3.69

summer_transit_plot <- ggplot()
summer_transit_plot <- summer_transit_plot + 
  geom_raster(data = summer_transit_df, aes(x = x, y = y, fill = layer))
summer_transit_plot <- summer_transit_plot + coord_equal()
summer_transit_plot <- summer_transit_plot + 
  scale_fill_gradientn('',
                       colours=c("red", "yellow","white", "skyblue", "darkblue"),
                       #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                       #labels=c("Minimum",0.5,"Maximum"),
                       values = rescale(c(min(summer_transit_df$layer),
                                          quantile_winter_transit_50,
                                          quantile_winter_transit_50 + 0.01,
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
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5),
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



# Fall transit (D)

grasslands_fall_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_grassland_unfiltered.tif')
forest_fall_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_forest_unfiltered.tif')
tundra_fall_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_tundra_unfiltered.tif')
shurbland_fall_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_shrubland_unfiltered.tif')
cropland_fall_transit <- 
  raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_cropland_unfiltered.tif')

fall_transit <- raster::merge(grasslands_fall_transit,forest_fall_transit,
                                tundra_fall_transit,shurbland_fall_transit,
                                cropland_fall_transit)



fall_transit_df <- data.frame(rasterToPoints(fall_transit))
head(fall_transit_df)

# quantile_fall_transit_50 <- quantile(fall_transit_df$layer,prob=0.5)
# 
# quantile_fall_transit_95 <- quantile(fall_transit_df$layer,prob=0.95)

fall_transit_df <- fall_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)

fall_transit_plot <- ggplot()
fall_transit_plot <- fall_transit_plot + 
  geom_raster(data = fall_transit_df, aes(x = x, y = y, fill = layer))
fall_transit_plot <- fall_transit_plot + coord_equal()
fall_transit_plot <- fall_transit_plot + 
  scale_fill_gradientn('',
                       colours=c("red", "yellow","white", "skyblue", "darkblue"),
                       #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                       #labels=c("Minimum",0.5,"Maximum"),
                       values = rescale(c(min(fall_transit_df$layer),
                                          quantile_winter_transit_50,
                                          quantile_winter_transit_50 + 0.01,
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
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5),
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
png(height = 2000,width=4500,res=300,'Figures/october_2021/Figure_3_seasonal_transit.png')
ggarrange(winter_transit_plot, spring_transit_plot,summer_transit_plot,fall_transit_plot,
          ncol = 2,nrow=2)
dev.off()

?ggarrange

#-------------------------------------------------------------------------------
#figure 4: climate effects on annual transit (NEEDS WORK) -----


climate_list <-c('aridity','pet','ppt')
climate_list_annual_df <- list()
climate_list_min_df <- list()

for(i in climate_list){
 
  annual_climate_coef <- 
    read.csv(paste0('./../../../Data/Derived_data/Model_outputs/annual_coef_',i,'.csv')) 
  
  climate_list_annual_df[[i]] <- annual_climate_coef
  
  # min_climate_coef <- 
  #   read.csv.csv(paste0('./../../../Data/Derived_data/Model_outputs/minimum_coef_',i,'.csv')) 
  # 
  # climate_list_min_df[[i]] <- min_climate_coef
  
}


#annual transit

#C
aridity_annual <- ggplot(climate_list_annual_df$aridity,aes(x=climate_mean,fill=veg)) +
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
pet_annual <- ggplot(climate_list_annual_df$pet,aes(x=climate_mean,fill=veg)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-.6,0.5)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('grassland'='blue','forest'='maroon',
                             'tundra'='grey','shrubland'='gold','cropland'='black'),
                    labels=c('grassland'='Grassland','forest'='Forest',
                             'tundra'='Tundra','shrubland'='Shrubland','cropland'='Cropland')) +
  xlab('Effect of PET on annual transit time') +
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
map_annual <- ggplot(climate_list_annual_df$ppt,aes(x=climate_mean,fill=veg)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-.0011,0.0002)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('grassland'='blue','forest'='maroon',
                             'tundra'='grey','shrubland'='gold','cropland'='black'),
                    labels=c('grassland'='Grassland','forest'='Forest',
                             'tundra'='Tundra','shrubland'='Shrubland','cropland'='Cropland')) +
  xlab('Effect of MAP on annual transit time') +
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
png(height = 1500,width=5500,res=300,'Figures/october_2021/Figure_4_coefs.png')


# png('Tables_Figures/coefficient_distributions_figure.png',
#     width=2550,height=700,res=150)
# 
# pdf('Figures/coefficient_distributions_figure.pdf',
#     width=17.5,height=4)

print(plot_grid(map_annual, pet_annual,aridity_annual,labels = c('A', 'B','C'),ncol = 3, nrow=1,
                rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 18))

dev.off()


#Next: do minimum trasit and see how it compares

