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

#Plot do four panel map of seasonality -------



