
veg_names <- c('Forest','Tundra','Shrubland')
veg_list <- list()

for(i in veg_names){

ground_estimates <- read.csv('./../../../woodwater/Data/site_WC_estimates.csv')
#head(ground_estimates)

#use only cross-checked sites
ground_estimates <- subset(ground_estimates,Exclude=='No')
unique(ground_estimates$Exclude)

#narrow down/rename columns
ground_estimates <-ground_estimates[c('Land.Cover.Type', 'Lat','Long','mean.moisture')]
colnames(ground_estimates) <- c('Land.Cover.Type','y','x','moisture.content')

head(ground_estimates)

#turn into normally gridded raster
ground_estimates <-ground_estimates[c(1,3,2,4)]
ground_estimates <- na.exclude(ground_estimates)

#vegetation
ground_estimates_vegetation <- subset(ground_estimates,Land.Cover.Type==i)

ground_estimates_vegetation <- fix_grid(ground_estimates_vegetation[c(2,3,4)])
#plot(ground_estimates_vegetation)

#re-import biomass data
dry_biomass_only<-raster('./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#get veg water in g/m^2:

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000

#since ground estimates are at coarser resolution, resample to that
dry_biomass_only <- resample(dry_biomass_only,ground_estimates_vegetation)

#merge them
ground_estimates_vegetation_df <-merge(data.frame(rasterToPoints(ground_estimates_vegetation)),
                                   data.frame(rasterToPoints(dry_biomass_only)),
                                   by=c('x','y'))
#head(ground_estimates_vegetation_df)
#length(unique(ground_estimates_df$layer)) 

#convert water content to water in g/m^2
ground_estimates_vegetation_df$veg_water <- 
  ground_estimates_vegetation_df$layer*ground_estimates_vegetation_df$aboveground_dry_biomass_density_aggregate_30X

#convert from g/m^2 to mm/m^2
ground_estimates_vegetation_df$veg_water <- ground_estimates_vegetation_df$veg_water*.001
ground_estimates_vegetation_df <- ground_estimates_vegetation_df[c(1,2,5)]

#import VOD-> VWC raster and turn into data frame (repeated code)
vwc_from_vod <- raster( './../../../Data/Derived_Data/VWC/Annual/annual_storage_vwc_global_unfiltered.tif')

#reample this to the empirical dataset
vwc_from_vod <- resample(vwc_from_vod,ground_estimates_vegetation)

#merge with ground-based df
ground_estimates_vegetation_df <- merge(ground_estimates_vegetation_df,data.frame(rasterToPoints(vwc_from_vod)))
ground_estimates_vegetation_df$veg <- i

veg_list[[i]] <- ground_estimates_vegetation_df

}

veg_list_df <- do.call('rbind',veg_list)
plot(annual_storage_vwc_global_unfiltered~veg_water,data=veg_list_df)
head(veg_list_df)

#import grasslands (run the script)
head(ground_estimates_grassland_df_filtered)
ground_estimates_grassland_df_filtered <- ground_estimates_grassland_df_filtered[c(1,2,3,6)]
ground_estimates_grassland_df_filtered <- ground_estimates_grassland_df_filtered[c(1,2,4,3)]
ground_estimates_grassland_df_filtered$veg <- 'grassland'

all_veg_df <- rbind(ground_estimates_grassland_df_filtered,veg_list_df)

plot(annual_storage_vwc_global_unfiltered~veg_water,data=all_veg_df)

vwc_comp <- ggplot(all_veg_df,aes(veg_water,annual_storage_vwc_global_unfiltered,color=veg)) +
  geom_point(size=2) +
  #geom_smooth(method='lm',se=F) +
  geom_abline(slope=1) +
  #geom_text(aes(label=x),hjust=0,vjust=0)
  # scale_colour_manual(values=c('grassland'='blue','forest'='maroon',
  #                              'tundra'='grey','shrubland'='purple','cropland'='black'),
  #                     labels=c('grassland'='Grassland','forest'='Forest',
  #                              'tundra'='Tundra','shrubland'='Shrubland',
  #                              'cropland'='Cropland')) +
  # geom_hline(yintercept = 0.5,color='black',linetype='dashed') +
  # geom_hline(yintercept = 1,color='black',linetype='dashed') +
  xlab('Ground-based VWC (mm)') +
  ylab('Remotely sensed VWC (mm)') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.75,0.25),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

summary(lm(annual_storage_vwc_global_unfiltered~veg_water,data=ground_estimates_forest_df))

png(height = 2000,width=2500,res=300,'Figures/october_2021/storage_comparison.png')
vwc_comp
dev.off()

cor(all_veg_df$veg_water,all_veg_df$annual_storage_vwc_global_unfiltered)


