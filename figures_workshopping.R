

library(scico)
library(cowplot)

# seasonal ------

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
winter_transit_df$season <- '(A) Winter'
#head(winter_transit_df)

#quantile_winter_transit_95 <- quantile(winter_transit_df$layer,prob=0.95)

#quantile_winter_transit_95 <- quantile(winter_transit_df$layer,prob=0.90)
quantile_winter_transit_95 <- 100

winter_transit_df <- winter_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)
summary(winter_transit_df)

winter_transit_plot <- ggplot(winter_transit_df, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  scale_fill_scico('Transit time (days)',palette = 'batlow',direction=-1) +
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
    legend.position = c(0.12,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())




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
spring_transit_df$season <- '(B) Spring'
head(spring_transit_df)
summary(spring_transit_df)

# quantile_spring_transit_50 <- quantile(spring_transit_df$layer,prob=0.5)
# 
# quantile_spring_transit_95 <- quantile(spring_transit_df$layer,prob=0.95)

spring_transit_df <- spring_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)

spring_transit_plot <- ggplot(spring_transit_df, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  scale_fill_scico('Transit time (days)',palette = 'batlow',direction=-1) +
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
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())




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
summer_transit_df$season <- '(C) Summer'
head(summer_transit_df)

# quantile_summer_transit_50 <- quantile(summer_transit_df$layer,prob=0.5)
# 
# quantile_summer_transit_95 <- quantile(summer_transit_df$layer,prob=0.95)

summer_transit_df <- summer_transit_df %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95)

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
fall_transit_df$season <- '(D) Fall'
head(fall_transit_df)

# quantile_fall_transit_50 <- quantile(fall_transit_df$layer,prob=0.5)
# 
# quantile_fall_transit_95 <- quantile(fall_transit_df$layer,prob=0.95)

seasons <- rbind(winter_transit_df,spring_transit_df,summer_transit_df, fall_transit_df)
head(seasons)
unique(seasons$season)

fall_transit_df <- seasons %>%
  filter(layer > 0) %>%
  filter(layer < quantile_winter_transit_95) %>%
  mutate(across(season, factor, levels=c("(A) Winter","(B) Spring","(C) Summer",
                                         "(D) Fall"))) 


# make_labelstring <- function(mypanels) {
#   mylabels <- sapply(mypanels, 
#                      function(x) {LETTERS[which(mypanels == x)]})
#   
#   return(mylabels)
# }
# 
# label_panels <- ggplot2::as_labeller(make_labelstring)

png(height = 2000,width=3000,res=300,'Figures/october_2021/Figure_3_seasonal_transit.png')

  ggplot(fall_transit_df , aes(x = x, y = y, fill = layer)) + 
  facet_wrap(~season) +
  geom_raster() + 
  scale_fill_scico('Transit time (days)',palette = 'batlow',direction=-1) +
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
    strip.text = element_text(size=10,hjust=0),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

  dev.off()

# png(height = 3000,width=3000,res=300,'Figures/october_2021/Figure_1_storage.png')
# 
# print(plot_grid(storage_1,storage_2,
#                 labels = c('A', 'B'),ncol = 1, nrow=2,
#                 rel_widths = c(2,2), 
#                 rel_heights = c(1,1),label_size = 15))
# 
# dev.off()

#-------------------------------------------------------------------------------
# storage figure 1 with vod-vwc and pools panels ------
  
  
  #mean annual storage (A)
  
  #load in annual storage
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
  
  storage_1 <- ggplot(storage_df, aes(x = x, y = y, fill = layer)) + 
    geom_raster() + 
    scale_fill_scico('Aboveground water storage (mm)',palette = 'batlow',direction=-1) +
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
  
  
  #add with bivariate VOD VWC relationship
  #https://wilkelab.org/cowplot/articles/plot_grid.html
  
  plots <- align_plots(storage_1, vwc_vod_plot, align = 'v', axis = 'l')
  # then build the bottom row
  bottom_row <- plot_grid(plots[[2]], vwc_vod_plot, labels = c('B', 'C'), label_size = 15)
  
  
  png(height = 3500,width=4500,res=300,'Figures/october_2021/Figure_1_Storage_V2.png')
  
  # then combine with the top row for final plot
  plot_grid(plots[[1]], bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)
  
 dev.off()
  
  