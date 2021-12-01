

grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_cropland_unfiltered.tif')


global_unfilitered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                                    shrublands_unfiltered,tundras_unfiltered,
                                    croplands_unfiltered)
plot(global_unfilitered)
summary(global_unfilitered)

quantile_95 = quantile(var_df$layer,prob=0.95)
quantile_75 = quantile(x$layer,prob=0.75)
quantile_50 = quantile(x$layer,prob=0.55)
quantile_40 = quantile(x$layer,prob=0.40)
quantile_30 = quantile(x$layer,prob=0.30)
quantile_20 = quantile(x$layer,prob=0.20)
quantile_10 = quantile(x$layer,prob=0.10)
quantile_5 = quantile(x$layer,prob=0.05)



# colour_func <- function(x){
#   
#   quantile_90 = quantile(x$layer,prob=0.95)
#   quantile_75 = quantile(x$layer,prob=0.75)
#   quantile_50 = quantile(x$layer,prob=0.55)
#   quantile_40 = quantile(x$layer,prob=0.40)
#   quantile_30 = quantile(x$layer,prob=0.30)
#   quantile_20 = quantile(x$layer,prob=0.20)
#   quantile_10 = quantile(x$layer,prob=0.10)
#   quantile_5 = quantile(x$layer,prob=0.05)
#   
#   ifelse(x$layer > quantile_90, 'darkblue',
#          # ifelse(x$layer < quantile_75, 'cyan4',
#                 ifelse(x$layer > quantile_50, 'cadetblue3',
#                        # ifelse(x$layer > quantile_40, 'cadetblue', 
#                               # ifelse(x$layer < quantile_30, 'gold',
#                                      ifelse(x$layer > quantile_20, 'goldenrod2', 
#                                             ifelse(x$layer > quantile_10, 'darkorange3', 
#                                                    ifelse(x$layer > quantile_5, 'darkorange', "red")))))
# }

library(scales)
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")

var_df <- data.frame(rasterToPoints(global_unfilitered))
var_df <- var_df %>% filter(layer < quantile_95)
quantile_50_2 = quantile(var_df$layer,prob=0.50)
summary(var_df)
hist(var_df$layer)

str(var_df)
p <- ggplot()
p <- p + geom_raster(data = var_df , aes(x = x, y = y, fill = layer))
p <- p + coord_equal()
p <- p + scale_fill_gradientn(
  colours=c("red", "yellow","white", "skyblue", "darkblue"),
  #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
  #labels=c("Minimum",0.5,"Maximum"),
  values = rescale(c(min(var_df$layer),
                     quantile_50_2,
                     quantile_50_2 + 0.01,
                     max(var_df$layer)))) +
  xlab('') +
  ylab('') +
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
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

p

# p <- p + scale_fill_gradientn(colours = colour_func(var_df))
# p
