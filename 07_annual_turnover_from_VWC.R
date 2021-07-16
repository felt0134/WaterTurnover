#combine T and VWC data

source('05_Import_Storage_Transp_Data.R')
test.transp <- rbind(test.tundra,test.cropland,test.forest,test.grassland,test.shrubland)

#grasslands -----

test.grassland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.grassland)
# head(test.grassland.cumulative.transp)
# str(test.grassland.cumulative.transp)

# get rid of pixels where T is zero
test.grassland.cumulative.transp <- test.grassland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.grassland.cumulative.transp$canopy_transpiration_mm_m2 <- test.grassland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.grassland.cumulative.transp)

grasslandraster<-rasterFromXYZ(test.grassland.cumulative.transp)
crs(grasslandraster)
rm(test.grassland.cumulative.transp)


#load in grassland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,grasslandraster)
vwc.grassland <- mask(test.vwc,grasslandraster)
rm(test.vwc)
# vwc.grassland <- resample(vwc.grassland,grasslandraster)
# plot(vwc.grassland)
# plot(grasslandraster)

#try to stack them
stack.test<- raster::stack(vwc.grassland,grasslandraster)
#plot(stack.test)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.grasslands <- stack.test$new
transit.grasslands.df <- as.data.frame(rasterToPoints(transit.grasslands))
#summary(transit.grasslands.df)
#hist(transit.grasslands.df$new)

#save the unfiltered raster 
# transit.grasslands.df.unfiltered.raster <- rasterFromXYZ(transit.grasslands.df)
# crs(transit.grasslands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.grasslands.df.unfiltered.raster)
# writeRaster(transit.grasslands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')


#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.grasslands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.grasslands.df$new,probs=c(0.05)))

transit.grasslands.df <- transit.grasslands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.grasslands.df$Cover <- 'grasslands'

hist(transit.grasslands.df$new)
summary(transit.grasslands.df$new)

#change back to raster to plot
grassland.transit.annual <- rasterFromXYZ(transit.grasslands.df[c(1,2,3)])
plot(grassland.transit.annual)
rm(stack.test,transit.grasslands)

#done 


#-------------------------------------------------------------------------------
#forests ------

test.forest.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.forest)
# head(test.forest.cumulative.transp)
# str(test.forest.cumulative.transp)

# get rid of pixels where T is zero
test.forest.cumulative.transp <- test.forest.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.forest.cumulative.transp$canopy_transpiration_mm_m2 <- test.forest.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.forest.cumulative.transp)

forestraster<-rasterFromXYZ(test.forest.cumulative.transp)
rm(test.forest.cumulative.transp)


#load in forest VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,forestraster)
vwc.forest <- mask(test.vwc,forestraster)
rm(test.vwc)
# vwc.forest <- resample(vwc.forest,forestraster)
# plot(vwc.forest)
# plot(forestraster)

#try to stack them
stack.test<- raster::stack(vwc.forest,forestraster)
#plot(stack.test)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.forests <- stack.test$new
transit.forests.df <- as.data.frame(rasterToPoints(transit.forests))
#summary(transit.forests.df)
#hist(transit.forests.df$new)

#save the unfiltered raster
transit.forests.df.unfiltered.raster <- rasterFromXYZ(transit.forests.df)
crs(transit.forests.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
plot(transit.forests.df.unfiltered.raster)
writeRaster(transit.forests.df.unfiltered.raster,
            './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')

#STOPPED HERE 07/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.forests.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.forests.df$new,probs=c(0.05)))

transit.forests.df <- transit.forests.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.forests.df$Cover <- 'forests'

#change back to raster to plot
forest.transit.annual <- rasterFromXYZ(transit.forests.df[c(1,2,3)])
plot(forest.transit.annual)
rm(stack.test,transit.forests)

#-------------------------------------------------------------------------------
#shrublands ------

test.shrubland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.shrubland)
# head(test.shrubland.cumulative.transp)
# str(test.shrubland.cumulative.transp)

# get rid of pixels where T is zero
test.shrubland.cumulative.transp <- test.shrubland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.shrubland.cumulative.transp$canopy_transpiration_mm_m2 <- test.shrubland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.shrubland.cumulative.transp)

shrublandraster<-rasterFromXYZ(test.shrubland.cumulative.transp)
rm(test.shrubland.cumulative.transp)


#load in shrubland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,shrublandraster)
vwc.shrubland <- mask(test.vwc,shrublandraster)
rm(test.vwc)
# vwc.shrubland <- resample(vwc.shrubland,shrublandraster)
# plot(vwc.shrubland)
# plot(shrublandraster)

#try to stack them
stack.test<- raster::stack(vwc.shrubland,shrublandraster)
#plot(stack.test)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.shrublands <- stack.test$new
transit.shrublands.df <- as.data.frame(rasterToPoints(transit.shrublands))
#summary(transit.shrublands.df)
#hist(transit.shrublands.df$new)

# save the unfiltered raster
# transit.shrublands.df.unfiltered.raster <- rasterFromXYZ(transit.shrublands.df)
# crs(transit.shrublands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.shrublands.df.unfiltered.raster)
# writeRaster(transit.shrublands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')

#STOPPED HERE 7/15/2021


# filter out extreme values
high<-as.numeric(quantile(transit.shrublands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.shrublands.df$new,probs=c(0.05)))

transit.shrublands.df <- transit.shrublands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.shrublands.df$Cover <- 'shrublands'

#change back to raster to plot
shrubland.transit.annual <- rasterFromXYZ(transit.shrublands.df[c(1,2,3)])
plot(shrubland.transit.annual)
rm(stack.test,transit.shrublands)



#-------------------------------------------------------------------------------
#tundras ------
test.tundra.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.tundra)
# head(test.tundra.cumulative.transp)
# str(test.tundra.cumulative.transp)

# get rid of pixels where T is zero
test.tundra.cumulative.transp <- test.tundra.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.tundra.cumulative.transp$canopy_transpiration_mm_m2 <- test.tundra.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.tundra.cumulative.transp)

tundraraster<-rasterFromXYZ(test.tundra.cumulative.transp)
crs(tundraraster) <- '+proj=longlat +datum=WGS84'
rm(test.tundra.cumulative.transp)


#load in tundra VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,tundraraster)
vwc.tundra <- mask(test.vwc,tundraraster)
rm(test.vwc)
# vwc.tundra <- resample(vwc.tundra,tundraraster)
# plot(vwc.tundra)
# plot(tundraraster)

#try to stack them
stack.test<- raster::stack(vwc.tundra,tundraraster)
#plot(stack.test)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.tundras <- stack.test$new
transit.tundras.df <- as.data.frame(rasterToPoints(transit.tundras))
#summary(transit.tundras.df)
#hist(transit.tundras.df$new)

#save the unfiltered raster 
transit.tundras.df.unfiltered.raster <- rasterFromXYZ(transit.tundras.df)
crs(transit.tundras.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
plot(transit.tundras.df.unfiltered.raster)
writeRaster(transit.tundras.df.unfiltered.raster,
            './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')

#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.tundras.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.tundras.df$new,probs=c(0.05)))

transit.tundras.df <- transit.tundras.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.tundras.df$Cover <- 'tundras'

#change back to raster to plot
tundra.transit.annual <- rasterFromXYZ(transit.tundras.df[c(1,2,3)])
plot(tundra.transit.annual)
rm(stack.test,transit.tundras)

#-------------------------------------------------------------------------------
#croplands -----

test.cropland.cumulative.transp <- aggregate(canopy_transpiration_mm_m2~x+y,sum,data=test.cropland)
# head(test.cropland.cumulative.transp)
# str(test.cropland.cumulative.transp)

# get rid of pixels where T is zero
test.cropland.cumulative.transp <- test.cropland.cumulative.transp %>%
  dplyr::filter(canopy_transpiration_mm_m2 > .01)

test.cropland.cumulative.transp$canopy_transpiration_mm_m2 <- test.cropland.cumulative.transp$canopy_transpiration_mm_m2/365

#summary(test.cropland.cumulative.transp)

croplandraster<-rasterFromXYZ(test.cropland.cumulative.transp)
rm(test.cropland.cumulative.transp)


#load in cropland VWC data
outfile <- './../../../Data/Derived_data/VWC/'
#ecoregion_dir <- dir(outfile, full.names = T,pattern = "2016")

ecoregion_dir <- dir(outfile, full.names = T)
ecoregion_dir <- ecoregion_dir[-c(1,2,15)] #remove december 2016 and other extras

vwc.list<-list()
for(j in ecoregion_dir[1:12]){
  
  
  test<-fread(j)
  vwc.list[[j]] <- test
  
  
  
}

#make into data frame
test.vwc<- do.call("rbind", vwc.list)
rm(vwc.list,test)
#head(test.vwc)
test.vwc<-aggregate(vwc~x+y,mean,data=test.vwc)
#head(test.vwc)

test.vwc<-fix_grid(test.vwc)
#plot(test.vwc)

test.vwc <- resample(test.vwc,croplandraster)
vwc.cropland <- mask(test.vwc,croplandraster)
rm(test.vwc)

#try to stack them
stack.test<- raster::stack(vwc.cropland,croplandraster)
#plot(stack.test)

stack.test$new <- stack.test$layer/stack.test$canopy_transpiration_mm_m2
#plot(stack.test[c(1,2,3)])

transit.croplands <- stack.test$new
transit.croplands.df <- as.data.frame(rasterToPoints(transit.croplands))
#summary(transit.croplands.df)
#hist(transit.croplands.df$new)

#save the unfiltered raster 
# transit.croplands.df.unfiltered.raster <- rasterFromXYZ(transit.croplands.df)
# crs(transit.croplands.df.unfiltered.raster) <- '+proj=longlat +datum=WGS84'
# plot(transit.croplands.df.unfiltered.raster)
# writeRaster(transit.croplands.df.unfiltered.raster,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_croplands_unfiltered.tif')

#STOPPED HERE 7/15/2021

# filter out extreme values
high<-as.numeric(quantile(transit.croplands.df$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.croplands.df$new,probs=c(0.05)))

transit.croplands.df <- transit.croplands.df %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

transit.croplands.df$Cover <- 'croplands'

hist(transit.croplands.df$new)
summary(transit.croplands.df$new)

#change back to raster to plot
cropland.transit.annual <- rasterFromXYZ(transit.croplands.df[c(1,2,3)])
plot(cropland.transit.annual)
rm(stack.test,transit.croplands)

#done 


#-------------------------------------------------------------------------------



#import the unfiltered files and combine them all-----

#import raster
grasslands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_grassland_unfiltered.tif')
summary(rasterToPoints(grasslands_unfiltered))
forests_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_forest_unfiltered.tif')
summary(forests_unfiltered)
shrublands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_shrubland_unfiltered.tif')
summary(shrublands_unfiltered)
tundras_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_tundra_unfiltered.tif')
summary(tundras_unfiltered)
croplands_unfiltered <- raster('./../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_croplands_unfiltered.tif')
summary(croplands_unfiltered)


global_raster_unfiltered <- raster::merge(grasslands_unfiltered,forests_unfiltered,
                            shrublands_unfiltered,tundras_unfiltered,
                            croplands_unfiltered)
summary(global_raster_unfiltered)
#6.1 days

# writeRaster(global_raster_unfiltered,
#             './../../../Data/Derived_Data/Turnover/Annual/annual_transit_vwc_global_unfiltered.tif')



#combine all of them -----

transit.all <- rbind(transit.forests.df,transit.grasslands.df,transit.shrublands.df,
                    transit.tundras.df,transit.croplands.df)

summary(transit.all)
hist(transit.all$new)
head(transit.all)

# filter out extreme values
# high<-as.numeric(quantile(transit.all$new,probs=c(0.95)))
# low<-as.numeric(quantile(transit.all$new,probs=c(0.05)))
# 
# transit.all <- transit.all %>%
#   dplyr::filter(layer < high) %>%
#   dplyr::filter(layer > low)
# 

# 
# plot(rasterFromXYZ(transit.all[c(1,2,3)]),
#       main='Transit time of water in aboveground biomass (days)')

merge.test <- raster::merge(tundra.transit.annual,grassland.transit.annual,
                            forest.transit.annual,shrubland.transit.annual,
                            cropland.transit.annual)
plot(merge.test)

merge.test <- as.data.frame(rasterToPoints(merge.test))
head(merge.test)

#I am going to contrain the outliers here to help with the mapping:

# filter out extreme values
high<-as.numeric(quantile(merge.test$layer,probs=c(0.95)))
low<-as.numeric(quantile(merge.test$layer,probs=c(0.05)))

merge.test <- merge.test %>%
  dplyr::filter(layer < high) %>%
  dplyr::filter(layer > low)


# make figures

#convert back to raster
transit.all.raster <- rasterFromXYZ(merge.test)

crs(transit.all.raster) <-'+proj=longlat +datum=WGS84'

#save global raster
#writeRaster(transit.all.raster,'./../../../Data/Derived_data/VWC/global_transit_2016.tif')

#plot it out
summary(transit.all.raster)

delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')

col_breaks <- seq(0.0,22,by=2)
my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks< 6.1)),
               colorRampPalette(delPosColors)(sum(col_breaks> 6.1)))

# hist(transit.all.raster$layer)
# 
# png('Figures/2016_annual_transit_VWC.png',width=8,height=6,units="in",res=400)
# par(mar=c(1, 1, 1, 1))
# plot(transit.all.raster,
#      main='Transit time of water in aboveground biomass (days)',
#      breaks = col_breaks, col=my_colors, asp=1, 
#      xaxt = "n", yaxt = "n",bty="n")
# dev.off()

# distributions by land cover type
# merge.test.land.cover <- merge(merge.test,transit.all[c(1,2,4)],by=c('x','y'))
# head(merge.test.land.cover)

head(transit.all)

# filter out extreme values
high<-as.numeric(quantile(transit.all$new,probs=c(0.95)))
low<-as.numeric(quantile(transit.all$new,probs=c(0.05)))

transit.all <- transit.all %>%
  dplyr::filter(new < high) %>%
  dplyr::filter(new > low)

head(transit.all)
summary(transit.all)

# PDFs in base R
#get normal distribution of PPT


transit.all.cover <- unique(transit.all$Cover)
transit.all.cover.list<-list()

for(i in transit.all.cover){
  
  test<-subset(transit.all,Cover==i)
  # pdf<-get_pdf_df(subset)
  # #pdf$cover <- i
  transit.all.cover.list[[i]] <- test
  
  
}

head(transit.all.cover.list[2])


# forests_pdf <- get_pdf_df(transit.forests.df)
# grasslands_pdf<- get_pdf_df(transit.grasslands.df)
# shrubland_pdf<- get_pdf_df(transit.shrublands.df)

# pdf('PDF_rainfall.pdf',width=8,height=6)
# 
# plot(data.frame(transit.all.cover.list[1])$forests.toy.df,data.frame(transit.all.cover.list[1])$forests.y,type='l',col='blue')
# lines(grasslands_pdf$toy.df,grasslands_pdf$y,type='l',col='red',add=TRUE)

# make two panel figure

png(file='Figures/transt_map_and_distributions.png',
    width=2000,height=1800,res=200)

layout(matrix(1:2, ncol=1))
par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.1

#panel 1:

plot(transit.all.raster,
     main='',
     breaks = col_breaks, col=my_colors, asp=1, 
     xaxt = "n", yaxt = "n",bty="n")
mtext(expression(paste("Transit time (days)")),side=4,line= -.75,cex=1,
      outer=TRUE,adj=.78)
mtext("A",adj=0,cex = 1.25)

#panel 2:

#grasslands
plot(density(data.frame(transit.all.cover.list[2])$grasslands.new),col='red',lwd=3,
     xlim=c(0,22),ylim=c(0,0.6),xlab='Transit time of water in aboveground biomass (days)',
     ylab='Probability density',main='',cex.lab=1.25)
#forests
lines(density(data.frame(transit.all.cover.list[1])$forests.new),col='blue',lwd=3,add=TRUE)
#shrublands
lines(density(data.frame(transit.all.cover.list[3])$shrublands.new),col='black',lwd=3,add=TRUE)
#tundras
lines(density(data.frame(transit.all.cover.list[4])$tundras.new),col='grey70',lwd=3,add=TRUE)
#croplands
lines(density(data.frame(transit.all.cover.list[5])$croplands.new),col='goldenrod',lwd=3,add=TRUE)
legend(10, 0.5, legend=c("Forests","Grasslands", "Shrublands",
                             "Tundras","Croplands"),         #alpha legend: 0.015, 150
       col=c("blue", "red","black","grey70","goldenrod"), lty=1.25,lwd=5,cex=1.25,box.lty=0)
mtext('Transit time of water in aboveground biomass (days)',side=1,line=2,cex=1.25,outer=TRUE)
mtext('Probability density',side=2,line= 2.2,cex=1.25,outer=TRUE,adj=0.15)
mtext("B",adj=0,cex = 1.25)

dev.off()

#Now look at seasonality -----
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# old-----
#try mergeing as dataframes
test.vwc.df <- as.data.frame(rasterToPoints(vwc.grassland))
head(test.vwc.df)
str(test.vwc.df)
str(test.grassland.cumulative.transp)
# test.vwc.df$layer <- test.vwc.df$layer*1000 #you essentially don't need to concert from kg/m^2 to mm/m^2
# test.vwc.df$layer <- test.vwc.df$layer*.001

#merge vwc and T
merge.vwc.t <- merge(test.vwc.df,test.grassland.cumulative.transp,by=c('x','y'))
head(merge.vwc.t)


?resamplelibrary(ggplot2)

png('Figures/2016_annual_transit_VWC_density.png',width=8,height=6,units="in",res=400)
ggplot(transit.all ,aes(x=new,fill=Cover)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  # scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
  #                            california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
  #                   labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
  #                            california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab('Transit time of water in aboveground biomass (days)') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.38,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

