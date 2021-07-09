

#load in these scripts first
source('07_annual_turnover_from_VWC.R')

#load VWC data-----

outfile <- './../../../Data/Derived_data/VWC/'
ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  test.vwc<-aggregate(vwc~x+y,mean,data=test)
  test.vwc$month <- j
  test.vwc$month <- gsub('./../../../Data/Derived_data/VWC//vwc_2016_','',test.vwc$month)
  test.vwc$month <- gsub('.csv','',test.vwc$month)
  test.vwc$month <-as.numeric(as.character(test.vwc$month))
  
  vwc.list[[j]] <- test.vwc
  
  
  
}

#do a loop for each season for each land cover type-----


seasons <- c('jan_march','april_june','july_september','october_december')


#grasslands
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

#get minimum turnover (fastest turnover per site)
min_turnover_grasslands <- aggregate(turnover~x+y,min,data=seasons.grasslands.df)
# boxplot(min_turnover_grasslands_2$turnover)
# summary(min_turnover_grasslands)
#plot(rasterFromXYZ(min_turnover_grasslands))

#remove outliers based on IQR
min_turnover_grasslands_2<-filter_extremes_turnover_IQR(min_turnover_grasslands)
summary(min_turnover_grasslands_2)
min_turnover_grasslands_2$cover <- 'grasslands'

#forests
seasons.list.forest<-list()

for(i in seasons){
  
  
  test_turnover_function<-get_seasonal_turnover_VWC(season=i,
                                                    land_cover='forest')
  
  test_turnover_function$season <- i
  
  
  seasons.list.forest[[i]] <- test_turnover_function
  
  
}

seasons.forests.df <- do.call('rbind',seasons.list.forest)
rm(seasons.list.forest)
#head(seasoons.grasslands.df)

#get minimum turnover (fastest turnover per site)
# min_turnover_forests <- aggregate(turnover~x+y,min,data=seasons.forests.df)
# hist(min_turnover_forests$turnover)
# summary(min_turnover_forests)

#remove outliers based on IQR
min_turnover_forests_2<-filter_extremes_turnover_IQR(min_turnover_forests)
summary(min_turnover_forests_2)
min_turnover_forests_2$cover <- 'forests'

#
#

#croplands
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

#get minimum turnover (fastest turnover per site)
min_turnover_croplands <- aggregate(turnover~x+y,min,data=seasons.croplands.df)
# boxplot(min_turnover_croplands$turnover)
# summary(min_turnover_croplands)

#remove outliers based on IQR
min_turnover_croplands_2<-filter_extremes_turnover_IQR(min_turnover_croplands)
summary(min_turnover_croplands)
min_turnover_croplands_2$cover <- 'croplands'


#
#



#shrublands
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

#get minimum turnover (fastest turnover per site)
min_turnover_shrublands <- aggregate(turnover~x+y,min,data=seasons.shrublands.df)
# boxplot(min_turnover_shrublands$turnover)
# summary(min_turnover_shrublands)


#remove outliers based on IQR
min_turnover_shrublands_2<-filter_extremes_turnover_IQR(min_turnover_shrublands)
summary(min_turnover_shrublands)
min_turnover_shrublands_2$cover <- 'shrublands'


#
#



#tundra
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

#get minimum turnover (fastest turnover per site)
min_turnover_tundras <- aggregate(turnover~x+y,min,data=seasons.tundras.df)
# boxplot(min_turnover_tundras$turnover)
# summary(min_turnover_tundras)

#remove outliers based on IQR
min_turnover_tundras_2<-filter_extremes_turnover_IQR(min_turnover_tundras)
summary(min_turnover_tundras_2)
min_turnover_tundras_2$cover <- 'tundra'

# merging all together to look at minimum transit -----

rbind_min_transit <- rbind(min_turnover_tundras_2,min_turnover_croplands_2,
                           min_turnover_forests_2,min_turnover_grasslands_2,
                           min_turnover_shrublands_2)

head(rbind_min_transit)

png('Figures/2016_minimum_transit_boxplots_VWC.png',width=8,height=6,units="in",res=400)

boxplot(turnover~cover,data=rbind_min_transit,ylab='Minimum transit timee (days)',
        xlab='')
abline(h=1,col='red')
text(5,1.2,'One day')

dev.off()

#merge all seasonal estimates together to estimate seasonal transit -----

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

