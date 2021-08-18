
# supporting analyses and figures

# per-pixel sample size for transit time ------



transit_sample_size <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_global_transit_sample_size.tif')

png('Figures/Supporting/transit_time_sample_size.png',
    width=8,height=6,units="in",res=400)
plot(transit_sample_size,main='Sample Size (Number of months)')

dev.off()
#-------------------------------------------------------------------------------
#map out points of water content -----

#plot land covers
# use most recent year (2015) in the multi-year dataset of landcover
land_cover <- raster('./../../../Data/Land_cover/GLASS-GLC/GLASS-GLC_7classes_2015.tif')
plot(land_cover$GLASS.GLC_7classes_2015,col='white')
plot(xyzSP, add = TRUE, bg = xyzSP$layer, pch = 19, cex = 1)

forest<-raster('./../../../Data/Derived_data/Land_Cover_Distributions/Forest.tif')
# grassland<-resample(raster('./../../../Data/Derived_data/Land_Cover_Distributions/Grassland.tif'),forest)
# tundra<-resample(raster('./../../../Data/Derived_data/Land_Cover_Distributions/Tundra.tif'),forest)
shrubland<-resample(raster('./../../../Data/Derived_data/Land_Cover_Distributions/Shrubland.tif'),forest)
# cropland<-resample(raster('./../../../Data/Derived_data/Land_Cover_Distributions/Cropland.tif'),forest)

#fix crs
crs(forest) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# crs(grassland) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# crs(tundra) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(shrubland) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# crs(cropland) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

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

ground_estimates_df <- rasterToPoints(ground_estimates)
xyzSP <- SpatialPointsDataFrame(coords = data.frame(ground_estimates_df)[,1:2], 
                                data = data.frame(ground_estimates_df), 
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))


png('Figures/Supporting/water_content_sample_distributions.png',
    width=8,height=6,units="in",res=400)
plot(forest,col='lightblue')
# plot(grassland,col='yellow',add=TRUE)
# plot(tundra,col='grey',add=TRUE)
plot(shrubland,col='gold',add=TRUE)
# plot(cropland,col='green',add=TRUE)
plot(xyzSP, add = TRUE, bg = xyzSP$layer, pch = 19, cex = 0.75)
legend(0, -60, legend=c("Forests","Shrublands"),     
       col=c("lightblue", "gold"), lty=1.0,lwd=5,cex=1.1,box.lty=0)

dev.off()


forest.poly<-rasterToPolygons(forest)
plot(forest.poly)

#stopped here 8/18/2021
