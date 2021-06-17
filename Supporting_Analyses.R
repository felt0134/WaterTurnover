# Supporting Ananlyses:


# Look at the effect of different levels of aggregation ------



# import native resolution aboveground biomass data

agb_native<-raster('./../../../Data/Biomass/Global_Maps_C_Density_2010_1763/data/aboveground_biomass_carbon_2010.tif')
#plot(agb_native)




# amazon crop -----

#crop once
#amazon<-zoom()

# class      : Extent 
# xmin       : -84.62047 
# xmax       : -44.75007 
# ymin       : -10.13578 
# ymax       : 5.025789 

amazon_crop<-crop(agb_native,extent(amazon))
plot(amazon_crop)

#crop by forest
# forest.raster<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif')
# forest.raster<-crop(forest.raster,extent(amazon_crop))
# #plot(forest.raster)
# amazon_crop = resample(forest.raster,amazon_crop, "bilinear")
# amazon_crop<-mask(forest.raster,amazon_crop)
# #plot(amazon_crop)
# writeRaster(amazon_crop,'./../../../Data/Derived_data/Biomass/Land_Cover/amazon_toy_example.tif')

#levels of aggregations 
ag.list<-list()

ag.level <- c(5,10,15,20,30,60)

for(i in ag.level){

  ag <- aggregate(amazon_crop,fact=i)
  ag<-data.frame(rasterToPoints(ag))
  ag.list[[i]] <-ag
  
}

ag.stack<-stack(ag.list[c(5,10)])

#distributions for aggregations

# native
hist(amazon_crop$Forest,main='native resolution=300m')
summary(rasterToPoints(amazon_crop))

# 5
test<-data.frame(ag.list[5])
summary(test)
#hist(test$Forest)

# 10
test.10<-data.frame(ag.list[10])
summary(test.10)
#hist(test.10$Forest)

#15
test.15<-data.frame(ag.list[15])
summary(test.15)
#hist(test.15$Forest)

#20
test.20<-data.frame(ag.list[20])
summary(test.20)
#hist(test.20$Forest)

#30: this is the aggregation level we used
test.30<-data.frame(ag.list[30])
summary(test.30)
#hist(test.30$aboveground_biomass_carbon_2010)

#60: go a step further 
test.60<-data.frame(ag.list[60])
summary(test.60)
#hist(test.60$Forest)

# plot out summary stats

# distributions of Biomass: 

# Set up multi-panel plot and save
png(file='./../../../Results/Supporting_Analyses/Aggregation/amazon_aggregations_distributions.png',
    width=900,height=650,res=100)

layout(matrix(1:6, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

hist(amazon_crop$aboveground_biomass_carbon_2010,main='native resolution-300m',xlab='C Biomass')
hist(test$aboveground_biomass_carbon_2010,main='Aggregate by 5',xlab='C Biomass')
hist(test.10$aboveground_biomass_carbon_2010,main='Aggregate by 10',xlab='C Biomass')
hist(test.15$aboveground_biomass_carbon_2010,main='Aggregate by 15',xlab='C Biomass')
#hist(test.20$Forest,main='Aggregate=20',xlab='C Biomass')
hist(test.30$aboveground_biomass_carbon_2010,main='Aggregate by 30 (ET resolution)',xlab='C Biomass')
hist(test.60$aboveground_biomass_carbon_2010,main='Aggregate by 60',xlab='C Biomass')

dev.off()

#maps of biomass

png(file='./../../../Results/Supporting_Analyses/Aggregation/amazon_aggregations_maps.png',
    width=900,height=650,res=100)

layout(matrix(1:6, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

plot(amazon_crop,main='native resolution-300m')
plot(rasterFromXYZ(test),main='Aggregate by 5')
plot(rasterFromXYZ(test.10),main='Aggregate by 10')
plot(rasterFromXYZ(test.15),main='Aggregate by 15')
plot(rasterFromXYZ(test.30),main='Aggregate by 30 (ET resolution)')
plot(rasterFromXYZ(test.60),main='Aggregate by 60')

dev.off()

# Compare SD

#native
native.sd <- data.frame(sd(data.frame(rasterToPoints(amazon_crop))$aboveground_biomass_carbon_2010))
native.sd$aggregation <-'0'
colnames(native.sd) <- c('sd','aggregation')

#5
test.5.sd <- data.frame(sd(test$aboveground_biomass_carbon_2010))
test.5.sd$aggregation <-'5'
colnames(test.5.sd) <- c('sd','aggregation')

#15
test.15.sd <- data.frame(sd(test.15$aboveground_biomass_carbon_2010))
test.15.sd$aggregation <-'15'
colnames(test.15.sd) <- c('sd','aggregation')

#30
test.30.sd <- data.frame(sd(test.30$aboveground_biomass_carbon_2010))
test.30.sd$aggregation <-'30'
colnames(test.30.sd) <- c('sd','aggregation')

#60
test.60.sd <- data.frame(sd(test.60$aboveground_biomass_carbon_2010))
test.60.sd$aggregation <-'60'
colnames(test.60.sd) <- c('sd','aggregation')


rbind.sd <-rbind(native.sd,test.5.sd, test.15.sd,test.30.sd,test.60.sd)
rbind.sd$aggregation <- as.numeric(as.character(rbind.sd$aggregation))
str(rbind.sd)

png(file='./../../../Results/Supporting_Analyses/Aggregation/amazon_aggregations_spatial_variation.png',
    width=900,height=650,res=100)

barplot(sd~aggregation,data=rbind.sd,xlab='Aggregation',ylab='Spatial variaton (SD)')

dev.off()

#compare median

#native
native.median <- data.frame(median(data.frame(rasterToPoints(amazon_crop))$aboveground_biomass_carbon_2010))
native.median$aggregation <-'0'
colnames(native.median) <- c('median','aggregation')

#5
test.5.median <- data.frame(median(test$aboveground_biomass_carbon_2010))
test.5.median$aggregation <-'5'
colnames(test.5.median) <- c('median','aggregation')

#15
test.15.median <- data.frame(median(test.15$aboveground_biomass_carbon_2010))
test.15.median$aggregation <-'15'
colnames(test.15.median) <- c('median','aggregation')

#30
test.30.median <- data.frame(median(test.30$aboveground_biomass_carbon_2010))
test.30.median$aggregation <-'30'
colnames(test.30.median) <- c('median','aggregation')

#60
test.60.median <- data.frame(median(test.60$aboveground_biomass_carbon_2010))
test.60.median$aggregation <-'60'
colnames(test.60.median) <- c('median','aggregation')


rbind.median <-rbind(native.median,test.5.median, test.15.median,test.30.median,test.60.median)
rbind.median$aggregation <- as.numeric(as.character(rbind.median$aggregation))
str(rbind.median)

png(file='./../../../Results/Supporting_Analyses/Aggregation/amazon_aggregations_median.png',
    width=900,height=650,res=100)

barplot(median~aggregation,data=rbind.median,xlab='Aggregation',ylab='Median')

dev.off()


# now do another area (pacific northwest USA) ------



plot(agb_native)

#pnw <- zoom()

# class      : Extent 
# xmin       : -139.3679 
# xmax       : -106.0434 
# ymin       : 36.51519 
# ymax       : 61.00695 

pnw_crop<-crop(agb_native,extent(pnw))
plot(pnw_crop)

#levels of aggregations 
ag.list<-list()

ag.level <- c(5,10,15,20,30,60)

for(i in ag.level){
  
  ag <- aggregate(pnw_crop,fact=i)
  ag<-data.frame(rasterToPoints(ag))
  ag.list[[i]] <-ag
  
}


#distributions for aggregations

# native
hist(pnw_crop$aboveground_biomass_carbon_2010,main='native resolution=300m')
summary(rasterToPoints(pnw_crop))

# 5
pnw<-data.frame(ag.list[5])
summary(pnw)
#hist(pnw$Forest)

# 10
pnw.10<-data.frame(ag.list[10])
summary(pnw.10)
#hist(pnw.10$Forest)

#15
pnw.15<-data.frame(ag.list[15])
summary(pnw.15)
#hist(pnw.15$Forest)

#20
pnw.20<-data.frame(ag.list[20])
summary(pnw.20)
#hist(pnw.20$Forest)

#30: this is the aggregation level we used
pnw.30<-data.frame(ag.list[30])
summary(pnw.30)
#hist(pnw.30$aboveground_biomass_carbon_2010)

#60: go a step further 
pnw.60<-data.frame(ag.list[60])
summary(pnw.60)
#hist(pnw.60$Forest)

# plot out summary stats

# distributions of Biomass: 

# Set up multi-panel plot and save
png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_aggregations_distributions.png',
    width=900,height=650,res=100)

layout(matrix(1:6, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

hist(pnw_crop$aboveground_biomass_carbon_2010,main='native resolution-300m',xlab='C Biomass')
hist(pnw$aboveground_biomass_carbon_2010,main='Aggregate by 5',xlab='C Biomass')
hist(pnw.10$aboveground_biomass_carbon_2010,main='Aggregate by 10',xlab='C Biomass')
hist(pnw.15$aboveground_biomass_carbon_2010,main='Aggregate by 15',xlab='C Biomass')
#hist(pnw.20$Forest,main='Aggregate=20',xlab='C Biomass')
hist(pnw.30$aboveground_biomass_carbon_2010,main='Aggregate by 30 (ET resolution)',xlab='C Biomass')
hist(pnw.60$aboveground_biomass_carbon_2010,main='Aggregate by 60',xlab='C Biomass')

dev.off()

#maps of biomass

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_aggregations_maps.png',
    width=900,height=650,res=100)

layout(matrix(1:6, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

plot(pnw_crop,main='native resolution-300m')
plot(rasterFromXYZ(pnw),main='Aggregate by 5')
plot(rasterFromXYZ(pnw.10),main='Aggregate by 10')
plot(rasterFromXYZ(pnw.15),main='Aggregate by 15')
plot(rasterFromXYZ(pnw.30),main='Aggregate by 30 (ET resolution)')
plot(rasterFromXYZ(pnw.60),main='Aggregate by 60')

dev.off()

# Compare SD

#native
native.sd <- data.frame(sd(data.frame(rasterToPoints(pnw_crop))$aboveground_biomass_carbon_2010))
native.sd$aggregation <-'0'
colnames(native.sd) <- c('sd','aggregation')

#5
pnw.5.sd <- data.frame(sd(pnw$aboveground_biomass_carbon_2010))
pnw.5.sd$aggregation <-'5'
colnames(pnw.5.sd) <- c('sd','aggregation')

#15
pnw.15.sd <- data.frame(sd(pnw.15$aboveground_biomass_carbon_2010))
pnw.15.sd$aggregation <-'15'
colnames(pnw.15.sd) <- c('sd','aggregation')

#30
pnw.30.sd <- data.frame(sd(pnw.30$aboveground_biomass_carbon_2010))
pnw.30.sd$aggregation <-'30'
colnames(pnw.30.sd) <- c('sd','aggregation')

#60
pnw.60.sd <- data.frame(sd(pnw.60$aboveground_biomass_carbon_2010))
pnw.60.sd$aggregation <-'60'
colnames(pnw.60.sd) <- c('sd','aggregation')


rbind.sd <-rbind(native.sd,pnw.5.sd, pnw.15.sd,pnw.30.sd,pnw.60.sd)
rbind.sd$aggregation <- as.numeric(as.character(rbind.sd$aggregation))
str(rbind.sd)

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_aggregations_spatial_variation.png',
    width=900,height=650,res=100)

barplot(sd~aggregation,data=rbind.sd,xlab='Aggregation',ylab='Spatial variaton (SD)')

dev.off()

#compare median

#native
native.median <- data.frame(median(data.frame(rasterToPoints(pnw_crop))$aboveground_biomass_carbon_2010))
native.median$aggregation <-'0'
colnames(native.median) <- c('median','aggregation')

#5
pnw.5.median <- data.frame(median(pnw$aboveground_biomass_carbon_2010))
pnw.5.median$aggregation <-'5'
colnames(pnw.5.median) <- c('median','aggregation')

#15
pnw.15.median <- data.frame(median(pnw.15$aboveground_biomass_carbon_2010))
pnw.15.median$aggregation <-'15'
colnames(pnw.15.median) <- c('median','aggregation')

#30
pnw.30.median <- data.frame(median(pnw.30$aboveground_biomass_carbon_2010))
pnw.30.median$aggregation <-'30'
colnames(pnw.30.median) <- c('median','aggregation')

#60
pnw.60.median <- data.frame(median(pnw.60$aboveground_biomass_carbon_2010))
pnw.60.median$aggregation <-'60'
colnames(pnw.60.median) <- c('median','aggregation')


rbind.median <-rbind(native.median,pnw.5.median, pnw.15.median,pnw.30.median,pnw.60.median)
rbind.median$aggregation <- as.numeric(as.character(rbind.median$aggregation))
str(rbind.median)

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_aggregations_median.png',
    width=900,height=650,res=100)

barplot(median~aggregation,data=rbind.median,xlab='Aggregation',ylab='Median')

dev.off()


# larger effects in the PNW where there is big discrepancy in biomass (very right skewed). The next step will
# be to filter this just to forests, and see if the impacts are as big, as this crop, like amazon, also
# likely includes grasslands

# filter by land cover

forest_landcover<-raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Forest.tif')
crs(forest_landcover) <- '+proj=longlat +datum=WGS84 +no_defs'
plot(forest_landcover)

forest_landcover_crop <- crop(forest_landcover,extent(agb_native))
forest_landcover_crop_2 <- crop(forest_landcover_crop,extent(pnw_crop))
forest_landcover_crop_3 <- resample(forest_landcover_crop_2,pnw_crop)
pnw_forest_masked <- mask(pnw_crop,forest_landcover_crop_3)
plot(forest_landcover_crop_3)
plot(pnw_forest_masked)
plot(pnw_crop)
hist(pnw_forest_masked$aboveground_biomass_carbon_2010)


# now look at aggregation effects for just forest

#levels of aggregations 

  
ag.30.pnw.forest <- data.frame(rasterToPoints(aggregate(pnw_forest_masked,fact=30)))
ag.30.pnw.forest$aggregation <- '30'
summary(ag.30.pnw.forest)


#distributions for aggregations

# native
hist(pnw_crop$aboveground_biomass_carbon_2010,main='native resolution=300m')
summary(rasterToPoints(pnw_crop))
summary(data.frame(rasterToPoints((pnw_forest_masked))))

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_forest_aggregations_distributions.png',
    width=900,height=650,res=100)

layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

hist(pnw_forest_masked$aboveground_biomass_carbon_2010,main='native resolution-300m',xlab='C Biomass')
hist(ag.30.pnw.forest$aboveground_biomass_carbon_2010,main='Aggregate by 30',xlab='C Biomass')

dev.off()

#maps of biomass

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_forest_aggregations_maps.png',
    width=900,height=650,res=100)

layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(5, 4, 4, 2),pty='s')

plot(pnw_forest_masked,main='native resolution-300m')
plot(aggregate(pnw_forest_masked,fact=30),main='Aggregate by 30')

dev.off()
  
#compare median

#native
native.median.pnw.forest <- data.frame(median(data.frame(rasterToPoints(pnw_forest_masked))$aboveground_biomass_carbon_2010))
native.median.pnw.forest$aggregation <-'0'
colnames(native.median.pnw.forest) <- c('median','aggregation')

#30
pnw.forest.30.median <- data.frame(median(ag.30.pnw.forest$aboveground_biomass_carbon_2010))
pnw.forest.30.median$aggregation <-'30'
colnames(pnw.forest.30.median) <- c('median','aggregation')

rbind.median.pnw.forest <-rbind(pnw.forest.30.median,native.median.pnw.forest)
#rbind.median.pnw.forest$aggregation <- as.numeric(as.character(rbind.median.pnw.forest$aggregation))
#str(rbind.median.pnw.forest$aggregation)

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_forest_aggregations_median.png',
    width=900,height=650,res=100)

barplot(median~aggregation,data=rbind.median.pnw.forest,xlab='Aggregation',ylab='Median')

dev.off()

#compare SD

#native
native.sd.pnw.forest <- data.frame(sd(data.frame(rasterToPoints(pnw_forest_masked))$aboveground_biomass_carbon_2010))
native.sd.pnw.forest$aggregation <-'0'
colnames(native.sd.pnw.forest) <- c('sd','aggregation')

#30
pnw.forest.30.sd <- data.frame(sd(ag.30.pnw.forest$aboveground_biomass_carbon_2010))
pnw.forest.30.sd$aggregation <-'30'
colnames(pnw.forest.30.sd) <- c('sd','aggregation')

rbind.sd.pnw.forest <-rbind(pnw.forest.30.sd,native.sd.pnw.forest)
#rbind.median.pnw.forest$aggregation <- as.numeric(as.character(rbind.median.pnw.forest$aggregation))
#str(rbind.median.pnw.forest$aggregation)

png(file='./../../../Results/Supporting_Analyses/Aggregation/pnw_forest_aggregations_sd.png',
    width=900,height=650,res=100)

barplot(median~aggregation,data=rbind.median.pnw.forest,xlab='Aggregation',ylab='Standard deviation')

dev.off()



