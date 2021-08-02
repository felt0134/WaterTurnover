
# analysis

# VWC annual transit summary stats unfiltered ------

# annual turnover

#import raster
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_croplands_unfiltered.tif')



#get  summary stats
unfiltered_data <- c(grasslands_unfiltered,forests_unfiltered,shrublands_unfiltered,
                     tundras_unfiltered,croplands_unfiltered)
annual_unfiltered_list_median<-list()
annual_unfiltered_list_95ci<-list()

for(i in unfiltered_data){

unfiltered_df<-as.data.frame(rasterToPoints(i))
#head(unfiltered_df)
colnames(unfiltered_df) <- c('x','y','transit')

# get median
annual_median<-data.frame(round(median(unfiltered_df$transit),2))
annual_median$vegetation <- names(i)
colnames(annual_median) <- c('median','vegetation')

#get conf interval
annual_95 <- data.frame(round(error.95(unfiltered_df$transit),2))
annual_95$vegetation <- names(i)
colnames(annual_95) <- c('ci95','vegetation')

# merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
# summary(merge_annual_median_95)

annual_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
annual_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)

}

#bind to df
annual_unfiltered_median_df <- do.call('rbind',annual_unfiltered_list_median)
annual_unfiltered_95ci_df <- do.call('rbind',annual_unfiltered_list_95ci)

#merge both * '==' doesn't work, '=' does work.
merge_annual_median_95 <- merge(annual_unfiltered_median_df,
                                annual_unfiltered_95ci_df,by=c('vegetation'))

#cleanup
merge_annual_median_95$vegetation <- gsub('annual_transit_vwc_','',merge_annual_median_95$vegetation)
merge_annual_median_95$vegetation <- gsub('_unfiltered','',merge_annual_median_95$vegetation)

#save

write.csv(merge_annual_median_95,'Figures/annual_transit_summary_unfiltered.csv')



summary(grasslands_unfiltered)

#-------------------------------------------------------------------------------
# VWC minimum transit summary stats unfiltered ------

# annual turnover

#import raster
grasslands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_grasslands_minimum_transit.tif')
forests_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_forests_minimum_transit.tif')
shrublands_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_shrublands_minimum_transit.tif')
croplands_min <- raster( './../../../Data/Derived_Data/Turnover/Minimum/VWC_croplands_minimum_transit.tif')
tundras_min <- raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_tundras_minimum_transit.tif')



#get  summary stats
min_unfiltered_data <- c(grasslands_min,forests_min,shrublands_min,
                     tundras_min,croplands_min)
min_unfiltered_list_median<-list()
min_unfiltered_list_95ci<-list()

for(i in min_unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  # get median
  annual_median<-data.frame(round(median(unfiltered_df$transit),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','vegetation')
  
  #get conf interval
  annual_95 <- data.frame(round(error.95(unfiltered_df$transit),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','vegetation')
  
  # merge_annual_median_95 <- merge(annual_95,annual_median,by==c('vegetation'))
  # summary(merge_annual_median_95)
  
  min_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  min_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  
}

#bind to df
min_unfiltered_median_df <- do.call('rbind',min_unfiltered_list_median)
min_unfiltered_95ci_df <- do.call('rbind',min_unfiltered_list_95ci)

#merge both * '==' doesn't work, '=' does work.
merge_min_median_95 <- merge(min_unfiltered_median_df,
                                min_unfiltered_95ci_df,by=c('vegetation'))

#cleanup
merge_min_median_95$vegetation <- gsub('VWC_','',merge_min_median_95$vegetation)
merge_min_median_95$vegetation <- gsub('_minimum_transit','',merge_min_median_95$vegetation)

#save
write.csv(merge_min_median_95,'Figures/minimum_transit_summary_unfiltered.csv')

#-------------------------------------------------------------------------------
# Map out annual and minimum transit unfiltered ------

#import raster
global_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')
#global_unfiltered$new <- log10(global_unfiltered$annual_transit_vwc_global_unfiltered)
global_unfiltered_df <- data.frame(rasterToPoints(global_unfiltered))
head(global_unfiltered_df)
plot(global_unfiltered$new)
summary(global_unfiltered$new)

# #try ggplot mapping
# library("rnaturalearth")
# library("rnaturalearthdata")
# library('scico')
# library('ggplot2')
# library('viridis')
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# #log transformed
# png('Figures/2016_annual_transit_VWC_unfilitered_ggplot.png',width=8,height=6,units="in",res=400)
# ggplot(data = world) + geom_sf() + geom_point(data= global_unfiltered_df,aes(x=x, y=y, color = new),size=0.1) +
#   scale_color_gradient2(midpoint = 0.79, low = "blue", mid = "white",
#                         high = "red", space = "Lab" ) +
#   #scale_color_scico_d(alpha=1,begin=1,end=531,direction=1,palette = 'batlow',aesthetics = 'colour')
#   ggtitle('Annualized transit time (Log10 days)') +
#   theme(
#     axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
#     axis.text.y = element_text(color='black',size=10),
#     axis.title.x = element_text(color='black',size=10),
#     axis.title.y = element_text(color='black',size=10),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     legend.title = element_blank(),
#     #legend.text = element_text(size=2.25),
#     #legend.position = c(0.7,0.1),
#     legend.margin =margin(r=5,l=5,t=5,b=5),
#     legend.position = 'bottom',
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=10),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# dev.off()
# 
# summary(global_unfiltered_df)

#try these with base R 

#first plot raw values

delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')

# set breaks manually and assign colors
# minValue(rbind_diff); maxValue(rbind_diff) # look up min and max values
col_breaks <- c(0,1,2,3,4,5,6,7,8,9,10,20,50,224756.00)

my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks<6)),
               colorRampPalette(delPosColors)(sum(col_breaks>6)))

# length(my_colors)
# length(col_breaks)

png('Figures/2016_annual_transit_VWC_unfiltered_rawvalues_ggplot.png',width=8,height=6,units="in",res=400)
#?layout
layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,2),ncol=12))

par(mar=c(1, 1, 1, 1))
image(global_unfiltered$annual_transit_vwc_global_unfiltered, breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='Annualized Transit Time (days)')
#dev.off()

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors, min=min(col_breaks), max=max(col_breaks), nticks=length(col_breaks),  ticks=col_breaks, title="")

dev.off()
#done

#import minimum transit time raster


global_minimum_unfiltered<-raster('./../../../Data/Derived_Data/Turnover/Minimum/VWC_global_minimum_transit.tif')

summary(global_minimum_unfiltered)
#col_breaks_min <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10,20,50,2.411563e+04)
col_breaks_min <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10,20,5.375313e+06)

png('Figures/2016_minimum_transit_VWC_unfiltered_rawvalues_ggplot.png',width=8,height=6,units="in",res=400)

layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,2),ncol=12))

par(mar=c(1, 1, 1, 1))
image(global_minimum_unfiltered$VWC_global_minimum_transit, breaks = col_breaks_min, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='Minimum Transit Time (days)')
#dev.off()

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors, min=min(col_breaks_min), max=max(col_breaks_min), nticks=length(col_breaks_min),  ticks=col_breaks_min, title="")

dev.off()

#STOPPED HERE 7/19/2021




#-------------------------------------------------------------------------------
# VWC seasonal transit summary stats unfiltered ----


#import rasters

#winter
global_winter_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/winter_transit_vwc_global_unfiltered.tif')
global_winter_unfiltered_df <- data.frame(rasterToPoints(global_winter_unfiltered))
summary(global_winter_unfiltered_df)

#spring
global_spring_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/spring_transit_vwc_global_unfiltered.tif')
global_spring_unfiltered_df <- data.frame(rasterToPoints(global_spring_unfiltered))
summary(global_spring_unfiltered_df)

#summer
global_summer_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/summer_transit_vwc_global_unfiltered.tif')
global_summer_unfiltered_df <- data.frame(rasterToPoints(global_summer_unfiltered))
summary(global_summer_unfiltered_df)

#fall
global_fall_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Seasonal/fall_transit_vwc_global_unfiltered.tif')
global_fall_unfiltered_df <- data.frame(rasterToPoints(global_fall_unfiltered))
summary(global_fall_unfiltered_df)


#plot it out
delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')

my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks<6)),
               colorRampPalette(delPosColors)(sum(col_breaks>6)))

col_breaks <- c(0,1,2,3,4,5,6,10,15,20,25,50,100,1.326e+24)

png('Figures/2016_seasonal_transit_VWC_unfiltered_rawvalues_ggplot.png',width=8,height=6,units="in",res=400)

# layout(matrix(c(1,1,0,0,2,2,0,0),2,2))
# layout.show(2)

## allocate figure 2 the intersection of column 2 and row 2
# layout(matrix(c(1,2,5,3,4,5), 2, 3, byrow = TRUE))
# ## show the regions that have been allocated to each plot
# layout.show(5)

layout(matrix(c(1,1,1,1,2,2,2,2,5,#each break is a row in an 8x9 grid
                1,1,1,1,2,2,2,2,5,
                1,1,1,1,2,2,2,2,5,
                1,1,1,1,2,2,2,2,5,
                3,3,3,3,4,4,4,4,5,
                3,3,3,3,4,4,4,4,5,
                3,3,3,3,4,4,4,4,5,
                3,3,3,3,4,4,4,4,5), nrow = 8, ncol = 9, byrow = TRUE))

#layout(matrix(c(1,1,2,2,5,1,1,2,2,5,3,3,4,4,5,3,3,4,4,5), nrow = 4, ncol = 5, byrow = TRUE))

# winter
par(mar=c(0,0.5,0.5,1))
image(global_winter_unfiltered, breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='',xlab='',ylab='')
mtext("(A) Winter", side=3, line=-5, cex=1.2, adj=0)

# spring
par(mar=c(0,0.5,0.5,1))
image(global_spring_unfiltered, breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='',xlab='',ylab='')
mtext("(B) Spring", side=3, line=-5, cex=1.2, adj=0)

# summer
par(mar=c(0,0.5,0.5,1))
image(global_summer_unfiltered, breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='',xlab='',ylab='')
mtext("(C) Summer", side=3, line=-5, cex=1.2, adj=0)

# fall
par(mar=c(0,0.5,0.5,1))
image(global_fall_unfiltered, breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='',xlab='',ylab='')
mtext("(D) Fall", side=3, line=-5, cex=1.2, adj=0)

# color bar
#par(fig=c(0,7,6,10)/10)
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors, min=min(col_breaks), max=max(col_breaks), nticks=length(col_breaks),  ticks=col_breaks, title="Days")

dev.off()


#make a table sumamrizing global transit time by season

#get  summary stats
unfiltered_data <- c(global_winter_unfiltered,global_spring_unfiltered,
                     global_summer_unfiltered,global_fall_unfiltered)
seasonal_unfiltered_list_median<-list()
seasonal_unfiltered_list_95ci<-list()

for(i in unfiltered_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','transit')
  
  # get median
  annual_median<-data.frame(round(median(unfiltered_df$transit),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','season')
  
  #get conf interval
  annual_95 <- data.frame(round(error.95(unfiltered_df$transit),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','season')
  
  seasonal_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  seasonal_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  
}

#bind to df
seasonal_unfiltered_median_df <- do.call('rbind',seasonal_unfiltered_list_median)
seasonal_unfiltered_95ci_df <- do.call('rbind',seasonal_unfiltered_list_95ci)

#merge both * '==' doesn't work, '=' does work.
merge_seasonal_median_95 <- merge(seasonal_unfiltered_median_df,
                                  seasonal_unfiltered_95ci_df,by=c('season'))

#cleanup
merge_seasonal_median_95$season <- gsub('_transit_vwc_global_unfiltered','',merge_seasonal_median_95$season)

#save

write.csv(merge_seasonal_median_95,'Figures/seasonal_transit_summary_unfiltered.csv')




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#------------    STORAGE     ---------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# VWC annual storage summary stats unfiltered ----

# annual storage

#import raster
grasslands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_grassland_unfiltered.tif')
forests_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_forest_unfiltered.tif')
shrublands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_shrubland_unfiltered.tif')
tundras_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_tundra_unfiltered.tif')
croplands_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_cropland_unfiltered.tif')

#get  summary stats
unfiltered_storage_data <- c(grasslands_storage_unfiltered,forests_storage_unfiltered,
                             shrublands_storage_unfiltered,
                     tundras_storage_unfiltered,croplands_storage_unfiltered)
annual_storage_unfiltered_list_median<-list()
annual_storage_unfiltered_list_95ci<-list()

for(i in unfiltered_storage_data){
  
  unfiltered_df<-as.data.frame(rasterToPoints(i))
  #head(unfiltered_df)
  colnames(unfiltered_df) <- c('x','y','storage')
  
  # get median
  annual_median<-data.frame(round(median(unfiltered_df$storage),2))
  annual_median$vegetation <- names(i)
  colnames(annual_median) <- c('median','vegetation')
  
  #get conf interval
  annual_95 <- data.frame(round(error.95(unfiltered_df$storage),2))
  annual_95$vegetation <- names(i)
  colnames(annual_95) <- c('ci95','vegetation')

  annual_storage_unfiltered_list_median[[names(i)]] <- data.frame(annual_median)
  annual_storage_unfiltered_list_95ci[[names(i)]] <- data.frame(annual_95)
  
}

#bind to df
annual_storage_unfiltered_median_df <- do.call('rbind',annual_storage_unfiltered_list_median)
annual_storage_unfiltered_95ci_df <- do.call('rbind',annual_storage_unfiltered_list_95ci)

#merge both * '==' doesn't work, '=' does work.
merge_annual_storage_median_95 <- merge(annual_storage_unfiltered_median_df,
                                annual_storage_unfiltered_95ci_df,by=c('vegetation'))

#cleanup
merge_annual_storage_median_95$vegetation <- gsub('annual_storage_vwc_','',merge_annual_storage_median_95$vegetation)
merge_annual_storage_median_95$vegetation <- gsub('_unfiltered','',merge_annual_storage_median_95$vegetation)

#save

write.csv(merge_annual_storage_median_95,'Figures/annual_storage_summary_unfiltered.csv')


#-------------------------------------------------------------------------------
# Map out average and range of vegetation water storage --------------

#import storage
global_storage_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')
plot(global_storage_unfiltered)
summary(global_storage_unfiltered)


delNegColors= c("lightblue","darkblue")
delPosColors= c("brown","red",'rosybrown1')

# set breaks manually and assign colors
# minValue(rbind_diff); maxValue(rbind_diff) # look up min and max values
col_breaks <- c(0,0.5,1,1.5,2,2.5,3,4,5,6,7,8,9,10,11,12,13,14)

my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks<5)),
               colorRampPalette(delPosColors)(sum(col_breaks>5)))


length(col_breaks)
length(my_colors)

#import intra-annual range
global_storage_range_unfiltered <- raster('./../../../Data/Derived_Data/VWC/Range/VWC_global_range_storage.tif')
plot(global_storage_range_unfiltered)
summary(global_storage_range_unfiltered)

col_breaks_range <- c(0,0.25,0.5,0.75,1,1.5,2,2.5,3,9)

my_colors_range <- c(colorRampPalette(delNegColors)(sum(col_breaks_range<1)),
               colorRampPalette(delPosColors)(sum(col_breaks_range>1)))

length(col_breaks_range)
length(my_colors_range)

#try a multi-panel

png('Figures/2016_storage_unfilitered.png',
    width=8,height=6,units="in",res=400)
#?layout
layout(matrix(c(1,1,1,1,1,1,1,1,2,3,3,3,3,3,3,3,3,4),ncol=18))
#par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')

par(mar=c(1, 1, 3, 1))


# # Set up multi-panel plot
# layout(matrix(1:2, ncol=2))
# par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')
# 
# # Panel label setup
# line = 0.5
# cex = 1.5
# side = 3
# adj= 0.1

#storage
image(global_storage_unfiltered$annual_storage_vwc_global_unfiltered, 
      breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='')
mtext("(A) Vegetation Water Storage (mm)", side=3, line=0, cex=1.2, adj=0)
#dev.off()

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors, min=min(col_breaks), max=max(col_breaks), nticks=length(col_breaks), 
          ticks=col_breaks, title="")

#variability
image(global_storage_range_unfiltered$VWC_global_range_storage, 
      breaks = col_breaks_range, col=my_colors_range, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='')
mtext("(B) Intra-annual Range (mm)", side=3, line=0, cex=1.2, adj=0)
#dev.off()

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors_range, min=min(col_breaks_range), max=max(col_breaks_range),
          nticks=length(col_breaks_range), 
          ticks=col_breaks_range, title='')


dev.off()

# try each individually

# average storage:

png('Figures/2016_storage_unfilitered)single_panel.png',
    width=8,height=6,units="in",res=400)
layout(matrix(c(1,1,1,1,1,1,1,1,2),ncol=9))
#par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')

par(mar=c(1, 1, 3, 1))
image(global_storage_unfiltered$annual_storage_vwc_global_unfiltered, 
      breaks = col_breaks, col=my_colors, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='')
mtext("Vegetation Water Storage (mm)", side=3, line=0, cex=1.2, adj=0)

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors, min=min(col_breaks), max=max(col_breaks), nticks=length(col_breaks), 
          ticks=col_breaks, title="")

dev.off()

#variability in storage:

png('Figures/2016_storage_variability_unfilitered_single_panel.png',
    width=8,height=6,units="in",res=400)
layout(matrix(c(1,1,1,1,1,1,1,1,2),ncol=9))

#variability
image(global_storage_range_unfiltered$VWC_global_range_storage, 
      breaks = col_breaks_range, col=my_colors_range, asp=1, 
      xaxt = "n", yaxt = "n",bty="n",main='')
mtext("Intra-annual Range in Water Storage (mm)", side=3, line=0, cex=1.2, adj=0)
#dev.off()

# color bar
par(mar=c(1, 1, 3, 1))
color.bar(lut=my_colors_range, min=min(col_breaks_range), max=max(col_breaks_range),
          nticks=length(col_breaks_range), 
          ticks=col_breaks_range, title='')


dev.off()

#see how much water is stored in earth

global_storage_unfiltered <- data.frame(rasterToPoints(global_storage_unfiltered))
global_storage_unfiltered$multiplied <- 
  global_storage_unfiltered$annual_storage_vwc_global_unfiltered*9e+6
head(global_storage_unfiltered)

sum(global_storage_unfiltered$multiplied)
#64208231251

test<- aggregate(annual_storage_vwc_global_unfiltered~x+y,get_km_cubed,data=global_storage_unfiltered)
head(test)
sum(test$annual_storage_vwc_global_unfiltered)
#convert to gallons
64208231251*0.264172





