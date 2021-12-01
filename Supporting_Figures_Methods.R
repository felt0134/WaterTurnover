

#Supporting figures for methods



# Sample size for transit time (# of months) -----

grasslands_sample_size <- raster('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_grassland_transit_sample_size.tif')
forests_sample_size <- raster('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_forest_transit_sample_size.tif')
shrublands_sample_size <- raster('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_shrubland_transit_sample_size.tif')
tundras_sample_size <- raster('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_tundra_transit_sample_size.tif')
croplands_sample_size <- raster('./../../../Data/Derived_Data/Turnover/Sample_Size/VWC_cropland_transit_sample_size.tif')

global_sample_size <- raster::merge(grasslands_sample_size,forests_sample_size,
                                    shrublands_sample_size,tundras_sample_size,
                                    croplands_sample_size)

#turn to data frame
sample_size_df <- data.frame(rasterToPoints(global_sample_size))

#get % of total possible pixels
sample_size_df$perc <-  (sample_size_df$layer/12)*100   
summary(sample_size_df)
#get median
quantile_50_2 = quantile(var_df$layer,prob=0.50)

sample_size_plot <- ggplot()
sample_size_plot <- sample_size_plot + geom_raster(data = sample_size_df , aes(x = x, y = y, fill = layer))
sample_size_plot <- sample_size_plot + coord_equal()
sample_size_plot <- sample_size_plot + scale_fill_gradientn('Sample size (Months)',
                                            colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                            #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                            #labels=c("Minimum",0.5,"Maximum"),
                                            values = rescale(c(min(sample_size_df$perc),
                                                               6,
                                                               6 + 0.01,
                                                               max(sample_size_df$perc)))) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
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


png(height = 2000,width=2000,res=300,'Figures/october_2021/Methods/Sample_Size.png')
sample_size_plot
dev.off()

#-------------------------------------------------------------------------------
# Uncertainty in quadrature -----


grasslands_uncertainty <- raster('./../../../Data/Derived_Data/Turnover/uncertainty/VWC_grassland_transit_uncertainty.tif')
forests_uncertainty <- raster('./../../../Data/Derived_Data/Turnover/uncertainty/VWC_forest_transit_uncertainty.tif')
shrublands_uncertainty <- raster('./../../../Data/Derived_Data/Turnover/uncertainty/VWC_shrubland_transit_uncertainty.tif')
tundras_uncertainty <- raster('./../../../Data/Derived_Data/Turnover/uncertainty/VWC_tundra_transit_uncertainty.tif')
croplands_uncertainty <- raster('./../../../Data/Derived_Data/Turnover/uncertainty/VWC_cropland_transit_uncertainty.tif')

global_uncertainty <- raster::merge(grasslands_uncertainty,forests_uncertainty,
                                    shrublands_uncertainty,tundras_uncertainty,
                                    croplands_uncertainty)

#turn to data frame
uncertainty_df <- data.frame(rasterToPoints(global_uncertainty))


uncertainty_50_2 = quantile(uncertainty_df$layer,prob=0.50)

uncertainty_plot <- ggplot()
uncertainty_plot <- uncertainty_plot + geom_raster(data = uncertainty_df , aes(x = x, y = y, fill = layer))
uncertainty_plot <- uncertainty_plot + coord_equal()
uncertainty_plot <- uncertainty_plot + scale_fill_gradientn('Temporal uncertainty',
                                                            colours=c("red", "yellow","white", "skyblue", "darkblue"),
                                                            #breaks=c(min(var_df$layer),median(var_df$layer),max(var_df$layer)),
                                                            #labels=c("Minimum",0.5,"Maximum"),
                                                            values = rescale(c(min(uncertainty_df$layer),
                                                                               uncertainty_50_2,
                                                                               uncertainty_50_2 + 0.01,
                                                                               max(uncertainty_df$layer)))) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
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


png(height = 2000,width=2000,res=300,'Figures/october_2021/Methods/uncertainty.png')
uncertainty_plot
dev.off()


#-------------------------------------------------------------------------------
# Autocorrelation/variograms before and after distance constraint (NEED TO X2 CHECK) ----

library(gstat)
library(splitstackshape)

test <- data.frame(climate_list_annual[1])
colnames(test) <- c('x', 'y', 'cover', 'transit', 'climate_mean')
test<- test %>%
  dplyr::filter(transit > 0)

test_forest<-subset(test,cover=='forest')

test_forest<-stratified(test_forest, c("y"), 0.01)
dim(test_forest) #522 pixels

#before subsampling
model_2_annual_transformed <- lm(log(transit)~climate_mean,data=test_forest)

#now look at autocorrelation and create spatial weight matrix
test_forest$Yresid <- resid(model_2_annual_transformed)
Vout_annual_1<-variogram(Yresid~1,loc=~x+y, width=.05,data=test_forest) ###calculate a variogram using the data. this will take a few annualutes if the full dataset
plot(Vout_annual_1$dist,Vout_annual_1$gamma,xlab='', ylab='Semivariance',main='',cex.lab=1.5, cex.main=2, pch=19, cex=.5)
abline(v=50,col='red')

# Example with distance-constrained subsampling
#distance constrained subsampling

test <- data.frame(climate_list_annual[1])
colnames(test) <- c('x', 'y', 'cover', 'transit', 'climate_mean')
test<- test %>%
  dplyr::filter(transit > 0)

test_forest<-subset(test,cover=='forest')

coordinates(test_forest) <- ~x+y
test_subsampled_poly <- subsample.distance(test_forest,size=100,d=50,replacement = T,
                                           latlong = T,echo = F)

#turn back into df and run model
test_subsampled <- as.data.frame(test_subsampled_poly)
transit_lm_subsampled<-lm(log(transit)~climate_mean,data=test_subsampled)

#now look at autocorrelation and create spatial weight matrix
test_subsampled$Yresid <- resid(transit_lm_subsampled)
Vout_annual_2<-variogram(Yresid~1,loc=~x+y, width=.05,data=test_subsampled) ###calculate a variogram using the data. this will take a few annualutes if the full dataset
plot(Vout_annual_2$dist,Vout_annual_2$gamma,xlab='Distance (km)', ylab='Semivariance',main='',cex.lab=1.5, cex.main=2, pch=19, cex=.5)

plot(test_subsampled_poly,pch=1,cex=0.5)

#plot it out

pdf('Figures/density_dependence_sonoran_multipanel.pdf',
    width=8.5,height=5)

png(height = 2000,width=2500,res=300,'Figures/october_2021/Methods/Variograms.png')

layout(matrix(1:2, ncol=2))
par(oma=c(1, 1, 1, 1), mar=c(1, 3, 1, 1),pty='s',mfrow=c(1,2))
?par
# Panel label setup
line = 0.75
cex = 1.0
side = 3
adj=-0.025

plot(Vout_annual_1$dist,Vout_annual_1$gamma,xlab='', 
     ylab='Semivariance',main='',cex.lab=1.5, cex.main=2, pch=19, cex=.5)
abline(v=50,col='red')
mtext('',side=3,line=0.50,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)
mtext('Distance (km)',side=1,line=-5,cex=1.5,outer=TRUE)
mtext('Semivariance',side=2,line=-0.5,cex=1.5,outer=TRUE)

plot(Vout_annual_2$dist,Vout_annual_2$gamma,xlab='', ylab='',main='',
     cex.lab=1.5, cex.main=2, pch=19, cex=.5)
mtext("B", side=side, line=line, cex=cex, adj=adj)

dev.off()


#-------------------------------------------------------------------------------
#land cover distribution ------


land_cover <- raster('./../../../Data/Land_cover/GLASS-GLC/GLASS-GLC_7classes_2015.tif')
plot(land_cover)

cropland.raster <- raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Cropland.tif')
forest.raster <- raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Forest.tif')
grassland.raster <- raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Grassland.tif')
shrubland.raster <- raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Shrubland.tif')
tundra.raster <- raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Tundra.tif')

merge.land.covers <- raster::merge(cropland.raster,forest.raster)
plot(merge.land.covers)

image(cropland.raster,col='black',legend=F)
image(forest.raster,add=T,col='maroon',legend=F)
image(grassland.raster,add=T,col='blue',legend=F)
image(shrubland.raster,add=T,col='gold',legend=F)
image(tundra.raster,col='grey',legend=F,add=T)
points(coord.check)
box()


shrubland.raster <- resample(shrubland.raster,grassland.raster)
plot(shrubland.raster,col='gold',legend=F)

grassland.poly <- readOGR('./../../../Data/Derived_data/Land_Cover_Distributions/shapefiles/grassland',
                          layer='grassland')
plot(grassland.poly)
library(rgeos)
grassland.poly.2 <- gSimplify(grassland.poly,tol=1)

map_extent <- extent(grassland.raster)

png(height = 2000,width=3000,res=300,'Figures/october_2021/Methods/Land_Cover_Dist.png')

#cropland
par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1))
plot(cropland.raster,
     maxpixels=ncell(cropland.raster),
     col='black',
     legend=FALSE,
     ext=map_extent)

#grassland
par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1),new=TRUE)
plot(grassland.raster,
     maxpixels=ncell(grassland.raster),legend=F,
     col='blue',
     ext=map_extent)

#shrubland
par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1),new=TRUE)
plot(shrubland.raster,
     maxpixels=ncell(shrubland.raster),legend=F,
     col='gold',
     ext=map_extent)

#forest
par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1),new=TRUE)
plot(forest.raster,
     maxpixels=ncell(forest.raster),legend=F,
     col='maroon',
     ext=map_extent)

#tundra
par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1),new=TRUE)
plot(tundra.raster,
     maxpixels=ncell(tundra.raster),legend=F,
     col='grey',
     ext=map_extent)

par(mfrow=c(1,1), cex=1, mar=c(1,1,1,1),new=TRUE)
legend(25, 125, legend=c("Grassland", "Forest",
                         "Shrubland","Cropland","Tundra"),         #alpha legend: 0.015, 150
       col=c("blue", "maroon","gold","black","grey"), lty=1,lwd=4,cex=1.0,box.lty=0)


dev.off()

