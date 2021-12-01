

##---seems to work ------

library(spatialEco)

veg_list <-c('grassland','forest','tundra','shrubland','cropland')
slopes_veg_annual_list <- list()
r.squared_veg_annual_list <- list()

for(i in veg_list){
  
  
#run the function
test_function <- get_climate_model_coefs(list=climate_list_annual,x=1,veg=i)

#get slopes
slopes_df <- data.frame(do.call('rbind',test_function[[1]]))
slopes_df$climate_mean <- as.numeric(slopes_df$climate_mean)

#get r.squared
r.squared_df <- data.frame(do.call('rbind',test_function[[2]]))
r.squared_df$summary.transit_lm..r.squared <- as.numeric(r.squared_df$summary.transit_lm..r.squared)



slopes_veg_annual_list[[i]] <- slopes_df
r.squared_veg_annual_list[[i]] <- r.squared_df

hist(slopes_df$climate_mean)
}

#collapse lists:

#slopes
slopes_veg_annual_df <- rbind(data.frame(slopes_veg_annual_list$grassland),
                              data.frame(slopes_veg_annual_list$forest),
                              data.frame(slopes_veg_annual_list$cropland),
                              data.frame(slopes_veg_annual_list$tundra),
                              data.frame(slopes_veg_annual_list$shrubland))
#fix column IDs
slopes_veg_annual_df$veg <- as.factor(as.character(slopes_veg_annual_df$veg))
slopes_veg_annual_df$coef <- as.factor(as.character(slopes_veg_annual_df$coef))
slopes_veg_annual_df$id <- as.factor(as.character(slopes_veg_annual_df$id))


# ggplot(slopes_veg_annual_df,aes(climate_mean,color=veg)) +
#   geom_density()

#r.squared

#collapse
r.squared_veg_annual_df <- rbind(data.frame(r.squared_veg_annual_list$grassland),
                             data.frame(r.squared_veg_annual_list$forest),
                             data.frame(r.squared_veg_annual_list$cropland),
                             data.frame(r.squared_veg_annual_list$tundra),
                             data.frame(r.squared_veg_annual_list$shrubland))

#fix
r.squared_veg_annual_df$veg <- as.factor(as.character(r.squared_veg_annual_df$veg))
r.squared_veg_annual_df$coef <- as.factor(as.character(r.squared_veg_annual_df$coef))
r.squared_veg_annual_df$id <- as.factor(as.character(r.squared_veg_annual_df$id))


#save both outputs to file:

write.csv(slopes_veg_annual_df,'./../../../Data/Derived_data/Model_outputs/annual_aridity_coef.csv')
write.csv(r.squared_veg_annual_df,'./../../../Data/Derived_data/Model_outputs/annual_aridity_r.squared.csv')


#  autocorrelation/variograms before and after distance constraint ----

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

