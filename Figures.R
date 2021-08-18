# Figures


# plot global turnover raster -----

# head(annual_turnover_by_vegetation)
# summary(annual_turnover_by_vegetation)
# hist(annual_turnover_by_vegetation$cumulative.annual.turnover)

high<-as.numeric(quantile(annual_turnover_by_vegetation$cumulative.annual.turnover,probs=c(0.975)))
low<-as.numeric(quantile(annual_turnover_by_vegetation$cumulative.annual.turnover,probs=c(0.025)))

annual_turnover_by_vegetation.constrained <- annual_turnover_by_vegetation %>%
  dplyr::filter(cumulative.annual.turnover  < high) %>%
  dplyr::filter(cumulative.annual.turnover  > low)

#summary(annual_turnover_by_vegetation.constrained)

#plot it out

delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')

col_breaks <- seq(0.0,10,by=1)
my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks< 2.5)),
               colorRampPalette(delPosColors)(sum(col_breaks> 2.5)))

ecoregion='Global_land_cover'
outfile <- paste0("Figures/",ecoregion,"_turnover_2016_global.png")
png(file=outfile,
    width=9,height=6,units="in",res=400)

par(mar=c(1, 1, 1, 1))

plot(rasterFromXYZ(annual_turnover_by_vegetation.constrained[c(1,2,5)]), breaks = col_breaks, col=my_colors, asp=1, 
     xaxt = "n", yaxt = "n",bty="n",main="2016 Vegetation Water Turnover (days)")

dev.off()

#-------------------------------------------------------------------------------
# plot distributions of turnover by land cover types ------

ecoregion='Global_land_cover'
outfile <- paste0("Figures/",ecoregion,"_turnover_2016_distributions.png")

png(file=outfile,
    width=8,height=6,units="in",res=400)

ggplot(annual_turnover_by_vegetation.constrained ,aes(x=cumulative.annual.turnover,fill=vegetation)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..))  +
  scale_fill_manual(values=c('Grassland'='green4','Forest'='lightblue',
                             'Tundra'='grey','Shrubland'='gold'))+
  xlab('Vegetation water turnover (days)') +
  ylab('Probability Density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.48,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

#-------------------------------------------------------------------------------
# CV of turnover by land cover type ------

high<-as.numeric(quantile(cv_turnover_by_vegetation$monthly_turnover,probs=c(0.975)))
low<-as.numeric(quantile(cv_turnover_by_vegetation$monthly_turnover,probs=c(0.025)))

cv_turnover_by_vegetation.constrained <- cv_turnover_by_vegetation %>%
  dplyr::filter(monthly_turnover  < high) %>%
  dplyr::filter(monthly_turnover  > low)

#summary(annual_turnover_by_vegetation.constrained)

#plot it out

delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')

summary(cv_turnover_by_vegetation.constrained)

col_breaks <- seq(0.0,2.8,by=0.2)
my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks< 1)),
               colorRampPalette(delPosColors)(sum(col_breaks> 1)))

ecoregion='Global_land_cover'
outfile <- paste0("Figures/",ecoregion,"_CV_turnover_2016_global.png")
png(file=outfile,
    width=9,height=6,units="in",res=400)

par(mar=c(1, 1, 1, 1))

plot(rasterFromXYZ(cv_turnover_by_vegetation.constrained[c(1,2,3)]), breaks = col_breaks, col=my_colors, asp=1, 
     xaxt = "n", yaxt = "n",bty="n",main="Variability Vegetation Water Turnover (CV)")

dev.off()

#-------------------------------------------------------------------------------
# plot distributions of CV of water turnover -----

ecoregion='Global_land_cover'
outfile <- paste0("Figures/",ecoregion,"_CV_turnover_2016_distributions.png")

png(file=outfile,
    width=8,height=6,units="in",res=400)

ggplot(cv_turnover_by_vegetation,aes(x=monthly_turnover,fill=vegetation)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..))  +
  scale_fill_manual(values=c('Grassland'='green4','Forest'='lightblue',
                             'Tundra'='grey','Shrubland'='gold'))+
  xlab('Variability vegetation water turnover (CV)') +
  ylab('Probability Density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.78,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

#-------------------------------------------------------------------------------
#Plot do four panel map of seasonality -------
#-------------------------------------------------------------------------------
#annual turnover and distributions ------

#prep

#get truncated annual turnover
land_covers <-c('grassland','forest','tundra','croplands','shrubland')
truncate_list <- list()
truncate_list_raster <-list()

for(i in land_covers){
  
  #just get dataframes
  test_trunc<-get_turncated_dist(i,annual=T)
  truncate_list[[i]]<-test_trunc
  
  #now get rasters
  test.trunc_raster <- rasterFromXYZ(test_trunc[c(1,2,3)])
  crs(test.trunc_raster) <- '+proj=longlat +datum=WGS84'
  
  truncate_list_raster[[i]] <- test.trunc_raster

  
}

#get rasters from the truncated distributions

global_truncated<-
  raster::merge(truncate_list_raster[1]$grassland,truncate_list_raster[2]$forest,
                truncate_list_raster[3]$tundra,truncate_list_raster[4]$croplands,
                truncate_list_raster[5]$shrubland)

summary(global_truncated)
hist(global_truncated)

#
#plot it out

delPosColors= c("lightblue","darkblue")
delNegColors= c("brown","red",'rosybrown1')
delhighColors= c("purple")
?seq
col_breaks <- seq(0,20,by=2)
# col_breaks_med <- seq(6,20,by=2)
# col_breaks_high <- seq(20,70,by=50)
?quantile
#col_breaks_quantile <- seq(quantile())
my_colors <- c(colorRampPalette(delNegColors)(sum(col_breaks < 5.8)),
               colorRampPalette(delPosColors)(sum(col_breaks < 20 & col_breaks > 5.8)))
               #colorRampPalette(delhighColors)(sum(col_breaks_high > 20)))

# ecoregion='Global_land_cover'
# outfile <- paste0("Figures/",ecoregion,"_turnover_2016_global.png")

png(file=outfile,
    width=9,height=6,units="in",res=400)



# Set up multi-panel plot
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0.5, 0.5, 0.5, 0.5))
#pty='s'
# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.1

plot(global_truncated, breaks = col_breaks, col=my_colors, box=F, asp=1.0,
     xaxt = "n", yaxt = "n",bty="n",main="2016 Vegetation Transit Time (days)",
legend.width=2, legend.shrink=.75)


png('Figures/annualized_transit_ecdfs.png',
    width=2500,height=2000,res=300)

plot(ecdf(data.frame(truncate_list[1])$grassland.turnover),
     xlab='Annualized transit time (days)',
     ylab='Probability density',main='',xlim=c(0,60))
plot(ecdf(data.frame(truncate_list[2])$forest.turnover),add=TRUE,col='red')
plot(ecdf(data.frame(truncate_list[3])$tundra.turnover),add=TRUE,col='grey')
plot(ecdf(data.frame(truncate_list[5])$shrubland.turnover),add=TRUE,col='blue')
plot(ecdf(data.frame(truncate_list[4])$croplands.turnover),add=TRUE,col='green')
abline(a=0.5,b=0,lty='dashed',col='grey',lwd=1)
legend(15, 0.30, legend=c("Grasslands","Forests","Tundras","Shrublands","Croplands"),     
       col=c("black", "red",'grey','blue',"green"), lty=1.0,lwd=5,cex=1.1,box.lty=0)

dev.off()



#other code for mapping


# Make colors
bks<- quantile(data.frame(rasterToPoints(global_truncated))$layer, probs=seq(0, 1, by=0.2), na.rm = TRUE)
#sensitivity=c("purple",'cyan3','green','yellow','orange','red')
sensitivity=c("blue",'lightblue','lightred', 'red')
bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
r.range.sens <- round(c(minValue(global_truncated), maxValue(global_truncated)),digits=2)

# Update projection
# proj4string(global_truncated) <- CRS("+proj=longlat")
# sensitivity_raster_2<-projectRaster(sensitivity_raster, crs=aea.proj)

png('Figures/global_transit_truncated.png',
    width=2200,height=1000,res=300)


# Set up multi-panel plot
layout(matrix(1:2, ncol=2),pty='s')
par(oma=c(2,2,2,2), mar=c(0.5, 4, 0.5, 4))
?par
#pty='s'
# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.1

plot(global_truncated,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,main='Observed',cex.main=1,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 5),
                    labels=seq(r.range.sens[1], r.range.sens[2], 5),
                    cex.axis=1.0),
     legend.args=list(text='', side=4, font=10, line=2.5, cex=0.7))
mtext("A", side=side, line=line, cex=cex, adj=adj)


plot(ecdf(data.frame(truncate_list[1])$grassland.turnover),
     xlab='Annualized transit time (days)',
     ylab='Probability density',main='',xlim=c(0,60))
plot(ecdf(data.frame(truncate_list[2])$forest.turnover),add=TRUE,col='red')
plot(ecdf(data.frame(truncate_list[3])$tundra.turnover),add=TRUE,col='grey')
plot(ecdf(data.frame(truncate_list[5])$shrubland.turnover),add=TRUE,col='blue')
plot(ecdf(data.frame(truncate_list[4])$croplands.turnover),add=TRUE,col='green')
abline(a=0.5,b=0,lty='dashed',col='grey',lwd=1)
legend(15, 0.30, legend=c("Grasslands","Forests","Tundras","Shrublands","Croplands"),     
       col=c("black", "red",'grey','blue',"green"), lty=1.0,lwd=5,cex=1.1,box.lty=0)
mtext("B", side=side, line=line, cex=cex, adj=adj)


dev.off()
