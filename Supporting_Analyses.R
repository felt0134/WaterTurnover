# Supporting Ananlyses:


# Look at the effect of different levels of aggregation ------



# import native resolution aboveground biomass data

agb_native<-raster('./../../../Data/Biomass/Global_Maps_C_Density_2010_1763/data/aboveground_biomass_carbon_2010.tif')
#plot(agb_native)


#crop to the amazon:

#amazon<-zoom()

amazon_crop<-crop(agb_native,extent(amazon))
#plot(amazon_crop)

# class      : Extent 
# xmin       : -84.62047 
# xmax       : -44.75007 
# ymin       : -10.13578 
# ymax       : 5.025789 

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

# stopped here

plot(agb_native)

#pnw <- zoom()
pnw_crop<-crop(agb_native,extent(pnw))
plot(pnw_crop)

#crop by forest
forest.raster_2<-raster('./../../../Data/Derived_data/Biomass/Land_Cover/Forest.tif')
forest.raster_2<-crop(forest.raster_2,extent(pnw_crop))
#plot(forest.raster_2)
pnw_crop = resample(forest.raster_2,pnw_crop, "bilinear")
#pnw_crop<-mask(forest.raster_2,pnw_crop)
#plot(pnw_crop)
writeRaster(pnw_crop,'./../../../Data/Derived_data/Biomass/Land_Cover/PNW_toy_example.tif')

look <- zoom()
look.crop <- crop(pnw_crop,extent(look))
plot(look.crop)
