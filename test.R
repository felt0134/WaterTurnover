# test code


#data repo
#http://afeldman.mit.edu.libproxy.chapman.edu/mt-dca-data

# from NEON support
library(rhdf5)
library(R.matlab)

#import lat lon data

lat_lon <- './../../../Data/VWC/SMAPCenterCoordinates9KM.mat'
#h5read(lat_lon, read.attributes = TRUE)
lat_lon<-readMat(lat_lon)

#lat
lat<-lat_lon[1]
lat<-as.data.frame(lat)

lat$ID <- rownames(lat)
lat <- reshape2::melt(lat, id.vars = c("ID"),variable.name = "lat")
lat <- lat[c(1,3)]
colnames(lat) <- c('ID','y')
lat$ID <- rownames(lat)
#lat$ID <- as.numeric(as.character(lat$ID))

#lon
lon<-lat_lon[2]
lon<-as.data.frame(lon)

lon$ID <- rownames(lon)
lon <- reshape2::melt(lon, id.vars = c("ID"),variable.name = "lon")
lon <- lon[c(1,3)]
colnames(lon) <- c('ID','x')
lon$ID <- rownames(lon)
#lon$ID <- as.numeric(as.character(lon$ID))

lat_lon_df <- merge(lon,lat,by=c('ID'))
rm(lat,lon,lat_lon)

# head(lat_lon_df)
# str(lon)
#done

# VOD files
#object_january_march <- './../../../Data/VWC/MTDCA_V4_TAU_201601_201603_9km.mat'
object_april_june <-'./../../../Data/VWC/MTDCA_V4_TAU_202004_202006_9km.mat'

#view the structure of the H5 file
#h5ls(object_april, all = TRUE)

# date data file
# dateInfo <- h5read(object_april, "DateVector", read.attributes = TRUE)
# dateInfo

# #tau data file
# otherInfo <- h5read(object, "MTDCA_SM", read.attributes = TRUE)
# str(otherInfo)
#
# openfile <- H5Fopen(object) # doesn't work

# read in date file
# date <-h5read(object_april_june,"DateVector")
# class(date) # "matrix" "array"
# dim(date) # 91 by 3: 91 rows (days), 'year', 'month','day' as columns


#load and save april-june VWC data ------

#april: 1-31
#may: 

get_vwc <-function(x,y,filepath){

april_vector <-c(2:3)
april_list<-list()

lat_lon <- './../../../Data/VWC/SMAPCenterCoordinates9KM.mat'
#h5read(lat_lon, read.attributes = TRUE)
lat_lon<-readMat(lat_lon)

#lat
lat<-lat_lon[1]
lat<-as.data.frame(lat)

lat$ID <- rownames(lat)
lat <- reshape2::melt(lat, id.vars = c("ID"),variable.name = "lat")
lat <- lat[c(1,3)]
colnames(lat) <- c('ID','y')
lat$ID <- rownames(lat)
#lat$ID <- as.numeric(as.character(lat$ID))

#lon
lon<-lat_lon[2]
lon<-as.data.frame(lon)

lon$ID <- rownames(lon)
lon <- reshape2::melt(lon, id.vars = c("ID"),variable.name = "lon")
lon <- lon[c(1,3)]
colnames(lon) <- c('ID','x')
lon$ID <- rownames(lon)
#lon$ID <- as.numeric(as.character(lon$ID))

lat_lon_df <- merge(lon,lat,by=c('ID'))
rm(lat,lon,lat_lon)


for(i in april_vector[2:3]){

# read in tau file 
tst.2 <-h5read(filepath,"MTDCA_TAU")
# dim(tst.2) # 1624 3856   91 (days)
# class(tst.2) # array
tst.2 <- tst.2[,,c(i)] # subset to day 1


#head(tst.2)

tst.2<-as.data.frame(tst.2)
tst.2 <- reshape2::melt(tst.2,variable.name = "tau")
# head(tst.2)
# summary(tst.2)
# hist(tst.2$value)

tst.2$ID <- rownames(tst.2)
#head(tst.2)
tst.2$vwc <- tst.2$value/0.11
tst.2 <- tst.2[c(3,4)]
#head(tst.2)

#merge with coordinates
tst.2 <- merge(lat_lon_df,tst.2,by=c('ID'))
tst.2 <- tst.2[c(2,3,4)]

#plot(lat_lon_df$x,lat_lon_df$y)

tst.2 <- tst.2 %>%
  dplyr::filter(!vwc=='NaN')
#head(vwc.coordinates)
#head(vwc.coord.jan1)

april_list[[i]]<-tst.2

}

#make to df
april_df <- do.call('rbind',april_list)
rm(april_list)
#head(april_df)

#average across pixels
april_ag<-aggregate(vwc~x+y,mean,data=april_df)
rm(april_df)
#head(april_ag)

return(april_ag)

}

test.get.vwc <- get_vwc(x=1,y=2,filepath='./../../../Data/VWC/MTDCA_V4_TAU_202004_202006_9km.mat')

write.csv(april_june_ag,'./../../../Data/Derived_data/VWC/vwc_2016_04.csv')

#
#

#Now do June

rm()


# extra ------

rbind.test <- bind(vwc.coord.jan4,vwc.coord.jan3, 
                   vwc.coord.jan2,vwc.coord.jan1,vwc.coord.jan5,vwc.coord.jan6,
                   vwc.coord.jan7)
head(rbind.test)

#png('Jan 3 vwc.png')
test.raster <- rasterFromXYZ(rbind.test,digits=0.4)
plot(test.raster,main='Jan 1-7')
dev.off()
?rasterFromXYZ

# stopped here

tst.2$vwc <- tst.2$value/0.11
tst.2 <- tst.2[c()]
hist(tst.2$vwc)
str(tst.2)

summary(lat_lon)

rm(tst.2)
dim(tst.2.3)

image(tst.2)
hist(tst.2)
min(tst.2)

tst.3.2.df <- as.data.frame(tst.2.3)
str(tst.3.2.df)
summary(tst.3.2.df)





###

vod.info <- h5readAttributes(object, "MTDCA_SM")
0.6/0.11

test.df <- as.data.frame(test)
head(test.df)
summary(test)

tmp <- h5ls(object)

test.3<-data.frame(matrix(tst.2, nrow=2, byrow=TRUE))

#old

# MTDCA2_9km_201601.mat
# test <- readMat('./../../../Data/VWC/MTDCA_V4_SM_201504_201506_9km.mat') #doesn't work 
# test <- readMat('./../../../Data/VWC/MTDCA2_9km_201601.mat') 

# ?readMat
# BiocManager::install("rhdf5")
# library(rhdf5)
# 
# h5ls('./../../../Data/VWC/MTDCA_V4_SM_201504_201506_9km.mat')
# test<-h5read('./../../../Data/VWC/MTDCA_V4_SM_201504_201506_9km.mat',name='MTDCA_SM')
# tst.2 <-h5read('./../../../Data/VWC/MTDCA_V4_SM_201504_201506_9km.mat',name='DateVector')
# head(test)
# str(test)

#data repo
#http://afeldman.mit.edu.libproxy.chapman.edu/mt-dca-data
