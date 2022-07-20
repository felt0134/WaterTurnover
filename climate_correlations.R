
#climate correlations
library(data.table)

#annual storage and turnover
source('annual_turnover_storage_import.r')

head(annual_turnover_lc)

group_2_names <- unique(annual_turnover_lc$group_2)

#import climate data
climate_data <- fread('./../../Data/climate/climate.csv')
climate_data <- na.omit(climate_data)
climate_data$mean_precipitation <- climate_data$mean_precipitation*365
climate_data$mean_pet <- climate_data$mean_pet*365
head(climate_data)

summary(climate_data)

#loop through each land cover to get correlations with mean climate
climate_cor_lc_list <- list()
for(j in group_2_names){
  
  #j = 'Evergreen needleleaf forest'
  
  group_2_lc <- subset(annual_turnover_lc,group_2==j)
  
  climate_data_lc <- merge(group_2_lc,climate_data,by=c('lat','lon'))
  
  #cor with pet
  cor_pet <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_pet,
                      method='spearman',exact=FALSE)
  pet_pval <- cor_pet$p.value
  pet_cor <- cor_pet$estimate
  
  #cor with ppt
  cor_ppt <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_precipitation,
                      method='spearman',exact=FALSE)
  ppt_pval <- cor_ppt$p.value
  ppt_cor <- cor_ppt$estimate
  
  #cor with aridity
  cor_aridity <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_aridity,
                          method='spearman',exact=FALSE)
  aridity_pval <- cor_aridity$p.value
  aridity_cor <- cor_aridity$estimate
  
  #cor with temp
  cor_temp <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_temp,
                          method='spearman',exact=FALSE)
  temp_pval <- cor_temp$p.value
  temp_cor <- cor_temp$estimate
  
  climate_val <- c('Potential Evapotranspiration','Precipitation','Aridity','Temperature')
  cor <- c(pet_cor,ppt_cor,aridity_cor,temp_cor)
  pval <- c(pet_pval,ppt_pval,aridity_pval,temp_pval)
  
  climate_cor_df <- data.frame(climate_val,cor,pval)
  
  climate_cor_df$land_cover <- j
  

  
  #store in list
  climate_cor_lc_list[[j]] <- climate_cor_df
  
}

level_order <- c("Savanna", "Cropland", "Deciduous broadleaf forest",
                 'Evergreen broadleaf forest','Grassland',
                 'Mixed forest','Evergreen needleleaf forest',
                 'Shrubland','Deciduous needleleaf forest')

climate_cor_lc_df <- do.call('rbind',climate_cor_lc_list)
head(climate_cor_lc_df,1)

climate_corrlations <- ggplot(climate_cor_lc_df,aes(x=factor(land_cover,level=level_order),
                              y=cor)) +
  facet_wrap(~climate_val,ncol=2) + 
  stat_summary(fun='mean',geom='bar') +
  ylab('Corrleation with annual transit time') +
  # scale_fill_manual(values=c('Savanna'='purple','Cropland'='darkblue',
  #                             'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
  #                    labels=c('Grassland'='Grassland','Forest'='Forest',
  #                             'Shrubland'='Shrubland','Savanna'='Savanna')) +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=8,angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    #legend.position = c(0.55,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 2000,width=3000,res=300,
    'manuscript_figures/climate_correlations_annual_transit.png')

print(climate_corrlations)

dev.off()


#minimum transit storage and climate correlation (workshopping) ------

source('minimum_turnover_import.r')
 head(minimum_turnover_lc,1)

#I bit the bullet and made one big .csv for this part
min_long <- fread('./../../Data/turnover_from_python/minimum/all_months.csv')
head(min_long)
min_long <- na.omit(min_long)
min_long_2 <- aggregate(Turnover ~lat+lon,min,data=min_long)
min_long_3 <- merge(min_long,min_long_2,by=c('lat','lon','Turnover'))
head(min_long_3,1)
rm(min_long_2,min_long_3)


lc_lat_lon <- minimum_turnover_lc %>%
  select(lat,lon,group)

min_long_3 <- merge(lc_lat_lon,min_long_3,by=c('lat','lon'))
head(min_long_3,1)


 transp_storage_min_plot <- ggplot(min_long_3,aes(y=transp_Wm2,
                                                      x=VWC,
                                                      color=group)) +
   geom_abline(slope=1,size=1,color='black',linetype="dashed") +
   #facet_wrap(~group,scales='free') + 
   scale_x_continuous(expand=c(0,0),limits=c(0,21)) +
   scale_y_continuous(expand=c(0,0)) +
   scale_color_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
   geom_point(size=0.05,alpha=0.025) +
   stat_smooth(method='lm',se=F,size=1) +
   ggtitle('During the month of minimum transit') +
   ylab('Canopy transpiration (mm/day)') +
   xlab(bquote('Aboveground water storage'~(mm/m^2))) +
   annotate("text", x=4, y=3, label= "1:1 Line") +
   theme(
     axis.text.x = element_text(color='black',size=15),
     axis.text.y = element_text(color='black',size=15),
     axis.title.x = element_text(color='black',size=22),
     axis.title.y = element_text(color='black',size=22),
     axis.ticks = element_line(color='black'),
     legend.key = element_blank(),
     legend.title = element_blank(),
     legend.key.size = unit(.50, 'cm'),
     legend.text = element_text(size=10),
     #legend.position = c(0.55,0.80),
     #legend.margin =margin(r=5,l=5,t=5,b=5),
     legend.position = 'top',
     strip.background =element_rect(fill="white"),
     strip.text = element_text(size=10),
     panel.background = element_rect(fill=NA),
     panel.border = element_blank(), #make the borders clear in prep for just have two axes
     axis.line.x = element_line(colour = "black"),
     axis.line.y = element_line(colour = "black"))
 
 
 png(height = 2000,width=2500,res=300,
     'manuscript_figures/storage_versus_transp_minimum_transit.png')
 
 print(transp_storage_min_plot)
 
 dev.off()
 
 
