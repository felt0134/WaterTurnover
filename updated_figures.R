
# figures and summary stats

#libraries
library(scico)
library(cowplot)

#import

#annual storage and turnover
source('annual_turnover_storage_import.r')

#minimum turnover
source('minimum_turnover_import.r')


#-------------------------------------------------------------------------------

# annual and minimum transit time by land cover types-------


group_2_names <- unique(annual_turnover_lc$group_2)
annual_turnover_list_2 <- list()

# right truncate for the figures

#annual truncate
for(j in group_2_names){
  
  group_2_lc <- subset(annual_turnover_lc,group_2==j)
  
  quantile_95 = quantile(group_2_lc$annual_turnover,probs=0.95) #by 95th percentile
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 90th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(annual_turnover < quantile_95) 
  
  #store in list
  annual_turnover_list_2[[j]] <- lc_filtered_2
  
}

annual_turnover_lc_95 <- do.call('rbind',annual_turnover_list_2)
rm(annual_turnover_list_2)

#minimum truncate
minimum_turnover_list_2 <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(minimum_turnover_lc,group_2==j)
  
  quantile_95 = quantile(group_2_lc$minimum_turnover,probs=0.95)
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 90th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(minimum_turnover < quantile_95) 
  
  #store in list
  minimum_turnover_list_2[[j]] <- lc_filtered_2
  
}

minimum_turnover_lc_95 <- do.call('rbind',minimum_turnover_list_2)
rm(minimum_turnover_list_2)

#to see order
#aggregate(annual_turnover ~ group,median,data=annual_turnover_lc_95)

annual_turnover_by_latitude <- 
  aggregate(annual_turnover ~ lat + group,median,data=annual_turnover_lc_95)
unique(annual_turnover_lc_95$group)

#transit by latitude
annual_turnover_by_lat_plot <- ggplot(annual_turnover_by_latitude,aes(lat,annual_turnover,col=group)) +
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',size=1,se=F) +
  geom_line(size=0.5,alpha=0.6) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Annual transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.85,0.25),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#break up land covers into facets
annual_turnover_by_lat_facet <- ggplot(annual_turnover_by_latitude,aes(lat,annual_turnover,col=group)) +
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',size=0.5,se=F) +
  facet_wrap(.~group) + 
  facet_wrap(ncol=1,~factor(group,levels=c('Savanna','Cropland','Grassland',
                                           'Forest','Shrubland'))) +
  geom_line(size=0.5,alpha=0.5) +
  coord_flip() +
  #scale_color_viridis_d() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Annual transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    #legend.position = c(0.7,0.6),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#minimum transit/turnover plots
minimum_turnover_by_latitude <- aggregate(minimum_turnover ~ lat + group,median,data=minimum_turnover_lc_95)

#minimum turnover by latitude
minimum_turnover_by_lat_plot <- ggplot(minimum_turnover_by_latitude,aes(lat,minimum_turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,minimum_turnover),col='black',size=1) +
  geom_line(size=0.4,alpha=0.5) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  #stat_summary(geom='line',fun='mean')
  ylab('Minimum transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    #legend.position = c(0.7,0.6),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#break up land covers into facets
minimum_turnover_by_lat_facet <- ggplot(minimum_turnover_by_latitude,aes(lat,minimum_turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,minimum_turnover),col='black',size=0.5) +
  facet_wrap(.~group) + 
  facet_wrap(ncol=1,~factor(group,levels=c('Savanna','Cropland','Grassland',
                                           'Forest','Shrubland'))) +
  geom_line(size=0.25,alpha=0.6) +
  coord_flip() +
  #scale_color_viridis_d() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Minimum transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    #legend.position = c(0.7,0.6),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 3000,width=3000,res=300,
    'manuscript_figures/transit_latitude_full_multipanel.png')

print(plot_grid(annual_turnover_by_lat_plot, annual_turnover_by_lat_facet,
                minimum_turnover_by_lat_plot, minimum_turnover_by_lat_facet,
                labels = c('a', 'b','c','d'),ncol = 2, nrow=2,
                rel_widths = c(1.25,1,1.25,1), 
                rel_heights = c(1,1,1,1),label_size = 20))


dev.off()

#cleanup
rm(annual_turnover_by_latitude,minimum_turnover_by_latitude,
   annual_turnover_by_lat_plot, annual_turnover_by_lat_facet,
   minimum_turnover_by_lat_plot, minimum_turnover_by_lat_facet)

#-------------------------------------------------------------------------------




# turnover boxplots -------


#aggregate(annual_turnover~group_2,median,data=annual_turnover_lc_95) # to find order

unique(annual_turnover_lc$group_2)
#summary(annual_turnover_lc)

#reorder by median transit
level_order <- c("Savanna", "Cropland", "Deciduous broadleaf forest",
                 'Evergreen broadleaf forest','Grassland',
                 'Mixed forest','Evergreen needleleaf forest',
                 'Shrubland','Deciduous needleleaf forest')

#plot
annual_turnover_boxplot <- ggplot(annual_turnover_lc_95,aes(x=factor(group_2,level=level_order),
                                                            y=annual_turnover,
                                                            color=annual_turnover)) +
  geom_hline(yintercept = 5.4) + #global median
  scale_color_scico('Annual transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1) +
  geom_boxplot(width=.1) +
  ylab('Annual transit time (days)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=8, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#min transit by land cover boxplot/violin plots
summary(minimum_turnover_lc)

#plot
minimum_turnover_boxplot <- ggplot(minimum_turnover_lc_95,aes(x=factor(group_2,level=level_order),
                                                              y=minimum_turnover,
                                                              color=minimum_turnover)) +
  geom_hline(yintercept = 2.37) + #global median
  scale_color_scico('Minimum transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1) +
  geom_boxplot(width=.1) +
  ylab('Minimum transit time (days)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=8, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 2500,width=2000,res=300,
    'manuscript_figures/transit_latitude_boxplot.png')


print(plot_grid(annual_turnover_boxplot, minimum_turnover_boxplot,
                labels = c('', ''),ncol = 1, nrow=2,
                rel_widths = c(1,1), 
                rel_heights = c(1,1),label_size = 20))

dev.off()

#cleanup
rm(annual_turnover_lc_95,minimum_turnover_lc_95,annual_turnover_boxplot, 
   minimum_turnover_boxplot)


#-------------------------------------------------------------------------------

# storage by land cover types-------


# truncate the condensed land cover types (group 2)
group_2_names <- unique(annual_turnover_lc$group_2)
storage_list_2 <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(annual_turnover_lc,group_2==j)
  
  quantile_95 = quantile(group_2_lc$annual_storage,probs=0.95)
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 90th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(annual_storage < quantile_95) 
  
  #store in list
  storage_list_2[[j]] <- lc_filtered_2
  
}


annual_storage_lc_95 <- do.call('rbind',storage_list_2)
rm(storage_list_2)

storage_by_latitude <- aggregate(annual_storage ~ lat + group,median,data=annual_storage_lc_95)

#storage by latitude
storage_by_lat <- ggplot(storage_by_latitude,aes(lat,annual_storage,col=group)) +
  stat_smooth(data=annual_storage_lc_95,aes(lat,annual_storage),col='black',size=1) +
  geom_line(size=0.25,alpha=0.75) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab(bquote('Aboveground water storage'~(mm/m^2))) +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.8,0.8),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#transit by land cover boxplot/violin plots

#reorder by median transit (in case you need again)
# level_order <- c("Savanna", "Cropland", "Deciduous broadleaf forest",
#                  'Evergreen broadleaf forest','Grassland',
#                  'Mixed forest','Evergreen needleleaf forest',
#                  'Shrubland','Deciduous needleleaf forest')

#summary(annual_turnover_lc)

#plot
boxplot_annual_storage <- ggplot(annual_storage_lc_95,aes(x=factor(group_2,level=level_order),
                                                          y=annual_storage,
                                                          color=annual_storage)) +
  geom_hline(yintercept = 3.58) + #global median
  scale_color_scico(bquote('Water storage'~(mm/m^2)),palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1) +
  geom_boxplot(width=.1) +
  ylab(bquote('Aboveground water storage'~(mm/m^2))) +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=8, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 2000,width=3500,res=300,
    'manuscript_figures/storage_multipanel.png')

print(plot_grid(storage_by_lat,boxplot_annual_storage,
                labels = c('a', 'b'),ncol = 2, nrow=1,
                rel_widths = c(1,1.5), 
                rel_heights = c(1,1),label_size = 25))

dev.off()


rm(storage_by_lat,boxplot_annual_storage,storage_by_latitude)


#-------------------------------------------------------------------------------
# summary stats of transit ------

#step 1: import original LC
annual_turnover_list <- list()
for(i in annual_turnover_dir[1:12]) {
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(annual_turnover > 0) %>%
    dplyr::filter(annual_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      './../../../Data/turnover_from_python/annual/land_cover_csvs//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  
  annual_turnover_list[[i]] <- lc_filtered
  
}

annual_turnover_lc <- do.call('rbind', annual_turnover_list)
rm(annual_turnover_list)

group_2_list <- unique(annual_turnover_lc$group_2)

annual_turnover_summary_list <- list()
for(j in group_2_list){
  
  #load in
  group_2_lc <- subset(annual_turnover_lc,group_2==j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$annual_turnover,probs=0.05),2)
  
  #05th quantile
  quantile_50 = round(quantile(group_2_lc$annual_turnover,probs=0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$annual_turnover,probs=0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$time <- 'annual'
  
  quantile_df$cover <- j
  
  quantile_df <- quantile_df %>%
    select(cover,quantile_50,quantile_5,quantile_95,time)
  
  #store in list
  annual_turnover_summary_list[[j]] <- quantile_df 
  
  
}


annual_turnover_summary <- do.call('rbind',annual_turnover_summary_list)
rm(annual_turnover_summary_list)
rownames(annual_turnover_summary) <- NULL
head(annual_turnover_summary,1)

write.csv(annual_turnover_summary, 'manuscript_figures/annual_turnover_summaries.csv')
rm(annual_turnover_summary,annual_turnover_lc)

#minimum turnover summary list

#step 1
minimum_turnover_list <- list()
for(i in minimum_turnover_dir[1:12]){
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(minimum_turnover > 0) %>%
    dplyr::filter(minimum_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      './../../../Data/turnover_from_python/minimum/land_cover_csvs//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  
  minimum_turnover_list[[i]] <- lc_filtered
  
}

minimum_turnover_lc <- do.call('rbind',minimum_turnover_list)
rm(minimum_turnover_list)

#step 2
minimum_turnover_summary_list <- list()
for(j in group_2_list){
  
  #load in
  group_2_lc <- subset(minimum_turnover_lc,group_2==j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$minimum_turnover,probs=0.05),2)
  
  #05th quantile
  quantile_50 = round(quantile(group_2_lc$minimum_turnover,probs=0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$minimum_turnover,probs=0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$cover <- j
  
  quantile_df$time <- 'minimum'
  
  quantile_df <- quantile_df %>%
    select(cover,quantile_50,quantile_5,quantile_95,time)
  
  
  #store in list
  minimum_turnover_summary_list[[j]] <- quantile_df 
  
  
}


minimum_turnover_summary <- do.call('rbind',minimum_turnover_summary_list)
rm(minimum_turnover_summary_list)
rownames(minimum_turnover_summary) <- NULL
head(minimum_turnover_summary,1)

write.csv(minimum_turnover_summary, 'manuscript_figures/minimum_turnover_summaries.csv')
rm(minimum_turnover_summary,minimum_turnover_lc)


#seasonal

#create a function that can input the name of the season and output the dataframe

seasonal_turnover_lc <- function(season,winter){
  
  lc_id <- read.csv('./../../../Data/land_cover_nsidc_ease2/land_cover_id.csv')  
  
  #set directories
  # season = 'winter'
  # winter = F
  filepath <- paste0('./../../../Data/turnover_from_python/seasonal/land_cover_csvs/',season,
                     '/')
  dir <- dir(filepath, full.names = T)
  dir <- dir[-c(11,13,15,16,17)] #remove land classes with no data
  
  #step 1 
  seasonal_turnover_summary_list <- list()
  for(i in dir[1:12]){
    
    #load in
    lc<-read.csv(i)
    
    if(winter==T){lc_filtered <- lc %>% #remove these in post, otherwise it screws things up for winter
      dplyr::filter(turnover > 0)}else{
        
        #filter out NA, inf, and zero values
        lc_filtered <- lc %>%
          dplyr::filter(lc01 != 'NA') %>% #only NAs for decidious needleleaf...
          dplyr::filter(turnover > 0) %>%
          dplyr::filter(turnover != 'Inf')}
    
    
    #get land cover ID
    name<-gsub(paste0('./../../../Data/turnover_from_python/seasonal/land_cover_csvs/',
                      season,'//landclass.'),'', i)
    name<-gsub('.3856x1624.bin.nc.csv',
               '', name)
    lc_filtered$class_number <- as.integer(name)
    lc_filtered <- merge(lc_filtered,lc_id,by=c('class_number'))
    
    #store in list
    seasonal_turnover_summary_list[[i]] <- lc_filtered
    
  }
  
  
  seasonal_turnover <- do.call('rbind',seasonal_turnover_summary_list)
  rm(seasonal_turnover_summary_list)
  
  
  group_2_list <- unique(seasonal_turnover$group_2)
  
  #step 2
  seasonal_turnover_summary_list_2 <- list()
  for(j in group_2_list){
    
    #load in
    group_2_lc <- subset(seasonal_turnover,group_2==j)
    
    #5th quantile
    quantile_5 = round(quantile(group_2_lc$turnover,probs=0.05),2)
    
    #05th quantile
    quantile_50 = round(quantile(group_2_lc$turnover,probs=0.5),2)
    
    #95th quantile
    quantile_95 = round(quantile(group_2_lc$turnover,probs=0.95),2)
    
    quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
    
    quantile_df$cover <- j
    
    quantile_df$time <- season
    
    quantile_df <- quantile_df %>%
      select(cover,quantile_50,quantile_5,quantile_95,time)
    
    
    #store in list
    seasonal_turnover_summary_list_2[[j]] <- quantile_df 
    
    
  }
  
  
  seasonal_turnover_summary <- do.call('rbind',seasonal_turnover_summary_list_2)
  rm(seasonal_turnover_summary_list_2)
  rownames(seasonal_turnover_summary) <- NULL
  
  return(seasonal_turnover_summary)
  
  
}

#winter
winter_summary <- seasonal_turnover_lc('winter',winter=T)
rownames(winter_summary) <- NULL
#head(winter_summary)

#spring
spring_summary <- seasonal_turnover_lc('spring',winter=F)
rownames(spring_summary) <- NULL

#summer
summer_summary <- seasonal_turnover_lc('summer',winter=F)
rownames(summer_summary) <- NULL

#fall
fall_summary <- seasonal_turnover_lc('fall',winter=F)
rownames(fall_summary) <- NULL

seasonal_summary <- rbind(winter_summary,spring_summary,
                          summer_summary,fall_summary)



write.csv(seasonal_summary, 'manuscript_figures/seasonal_summary.csv')


#now do storage





#-------------------------------------------------------------------------------
# summary stats of aboveground water storage -------

#set directories
annual_storage_filepath <- './../../../Data/turnover_from_python/annual/land_cover_csvs/'
annual_storage_dir <- dir(annual_storage_filepath, full.names = T)
annual_storage_dir <- annual_storage_dir[-c(11,13,15,16,17)]

storage_list <- list()
for(i in annual_storage_dir[1:12]){
  
  #load in
  lc<-read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(annual_storage > 0) %>%
    dplyr::filter(annual_storage != 'Inf')
  
  #get land cover ID
  name<-gsub('./../../../Data/turnover_from_python/annual/land_cover_csvs//landclass.',
             '', i)
  name<-gsub('.3856x1624.bin.nc.csv',
             '', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered,lc_id,by=c('class_number'))
  
  #store in list
  storage_list[[i]] <- lc_filtered
  
  
}


annual_storage_lc <- do.call('rbind',storage_list)

group_2_list <- unique(annual_storage_lc$group_2)

annual_storage_summary_list <- list()
for(j in group_2_list){
  
  #load in
  group_2_lc <- subset(annual_storage_lc,group_2==j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$annual_storage,probs=0.05),2)
  
  #05th quantile
  quantile_50 = round(quantile(group_2_lc$annual_storage,probs=0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$annual_storage,probs=0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$time <- 'annual'
  
  quantile_df$cover <- j
  
  quantile_df <- quantile_df %>%
    select(cover,quantile_50,quantile_5,quantile_95,time)
  
  #store in list
  annual_storage_summary_list[[j]] <- quantile_df 
  
  
}


annual_storage_summary <- do.call('rbind',annual_storage_summary_list)
rm(annual_storage_summary_list)
rownames(annual_storage_summary) <- NULL
head(annual_storage_summary,2)

write.csv(annual_storage_summary, 'manuscript_figures/annual_storage_summaries.csv')
rm(annual_storage_summary,annual_storage_lc)



#-------------------------------------------------------------------------------
# correlate ground with vod water storage ------


#import ground-based water content
ground_estimates <- read.csv('./../../woodwater/Data/site_WC_estimates.csv')

ground_estimates <- ground_estimates %>%
  dplyr::filter(Exclude=='No')

coords_ground <- ground_estimates[ , c("Long", "Lat")]   # coordinates
data   <- ground_estimates[,c("Land.Cover.Type","mean.moisture")]      # data
crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_ground_measurement <- SpatialPointsDataFrame(coords      = coords_ground,
                                                  data        = data, 
                                                  proj4string = crs)


#import aboveground biomass raster
dry_biomass_only <- raster('./../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

# plot(dry_biomass_only)
# points(Lat~Long,data=ground_estimates)

dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only<-dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only<-dry_biomass_only/10000
summary(dry_biomass_only)

#exctract biomass value from raster for each ground-based point coordinate
dry_biomass <- data.frame(extract(dry_biomass_only,coords_ground))
dry_biomass$id <- rownames(dry_biomass)
colnames(dry_biomass) <- c('dry_biomass','id')

ground_estimates$id <- rownames(ground_estimates)
ground_estimates_2 <- ground_estimates %>%
  select(mean.moisture,id)

#merge and calculate water storage
dry_biomass_ground_vwc <- merge(dry_biomass,ground_estimates,by=('id'))
dry_biomass_ground_vwc$ground_vwc <- 
  dry_biomass_ground_vwc$dry_biomass*dry_biomass_ground_vwc$mean.moisture
dry_biomass_ground_vwc$ground_vwc <- dry_biomass_ground_vwc$ground_vwc*0.001

#trim down columns
colnames(dry_biomass_ground_vwc)
dry_biomass_ground_vwc <- dry_biomass_ground_vwc %>%
  select(Long,Lat,ground_vwc,id)

#match up with VOD-based stuff

#turn ground data to spdf
coords_ground <- dry_biomass_ground_vwc[ , c("Long", "Lat")]   # coordinates
data_ground   <- data.frame(dry_biomass_ground_vwc[c(3:4)])          # data
crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

spdf_ground <- SpatialPointsDataFrame(coords      = coords_ground,
                                      data        = data_ground, 
                                      proj4string = crs)

#turn vod raster data to spatial points DF
annual_strorage_2 <- annual_turnover_lc %>%
  select(lon,lat,annual_storage,group,group_2)
rownames(annual_strorage_2) <- NULL

# prepare coordinates, data, and proj4string
coords_vod <- annual_strorage_2[ , c("lon", "lat")]   # coordinates
data_vod   <- annual_strorage_2[ , 3:5]          # data

# make the SpatialPointsDataFrame object
spdf_vod <- SpatialPointsDataFrame(coords      = coords_vod,
                                   data        = data_vod, 
                                   proj4string = crs)


library(FNN)

#link ground-based coordinates to vod coordinates for storage
nn1 = get.knnx(coordinates(spdf_vod), coordinates(spdf_ground), 1)
vector <- data.frame(nn1[1])
vector <- vector[c(1:37),]
spdf_vod_df <- data.frame(spdf_vod)
new_df <- spdf_vod_df[c(vector),]
new_df <- new_df %>%
  select(annual_storage,group,lon,lat)

#combine ground=based and vod-based water storage
spdf_ground_df <- data.frame(spdf_ground)
cbind_ground_vod <- cbind(new_df,dry_biomass_ground_vwc)
cbind_ground_vod <- cbind_ground_vod %>%
  dplyr::filter(!group==c('Water'),
                !group==c('Urban')) #don't want these two

#unique(cbind_ground_vod$group)

#metric of relationship between ground and VOD VWC
?cor
cor(cbind_ground_vod$annual_storage,cbind_ground_vod$ground_vwc,method='spearman')
#r=0.72
summary(lm(annual_storage~ground_vwc,data=cbind_ground_vod))
#slope = 0.58, R-squared = 0.46
mean((cbind_ground_vod$annual_storage-cbind_ground_vod$ground_vwc))
#bias = 0.84
vod_ground_lm <- lm(annual_storage~ground_vwc,data=cbind_ground_vod)
sqrt(mean(vod_ground_lm$residuals^2))
#RMSE = 2.12

vod_vwc_plot <- ggplot(cbind_ground_vod,
                       aes(annual_storage,ground_vwc,fill=group)) +
  scale_x_continuous(limits=c(0,12.9)) +
  scale_y_continuous(limits=c(0,12.9)) +
  geom_smooth(data=cbind_ground_vod,mapping=aes(annual_storage,ground_vwc,group=2),fullrange=T,
              method='lm',linetype='dashed',color='black',se=F,legend=F) +
  geom_point(size=5,pch=21) +
  scale_fill_manual(values=c('Cropland'='purple','Savanna'='darkblue',
                             'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                    labels=c('Grassland'='Grassland','Forest'='Forest',
                             'Shrubland'='Shrubland','Savanna'='Savanna')) +
  annotate("text", x=8.5, y=9.7, label= "1:1 Line") +
  annotate("text", x=10.5, y=8, label= "Slope") +
  annotate("text", x=2.25, y=10, label= "r = 0.74",size=8) +
  geom_abline(slope=1) +
  #geom_text(aes(label=x),hjust=0,vjust=0) +
  xlab(bquote('Satellite-based water storage'~(mm/m^2))) +
  ylab(bquote('Ground-based water storage'~(mm/m^2))) +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=19),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.8,0.25),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#compare to other pools/estimates

pools <- read.csv('./../../Data/Pools/H2OPoolSizeEstimates.csv')
head(pools)
pools$size <- as.numeric(as.character(pools$Size..km3.))
unique(pools$Pool)

#pool means
pool_means <- aggregate(Size..km3. ~ Pool,mean,data=pools)
16500.000/1094.914 #soil water (the next highest) is 15x greater than vegetation water)

vegetation_pools <- subset(pools,Pool=='Vegetation')
barplot(size~Citation,data=vegetation_pools)

mean(vegetation_pools$size)

this_study <- vegetation_pools %>%
  dplyr::filter(Citation=='This study')
str(this_study)

#pool_size <- ggplot(vegetation_pools, aes(x = size, y = reorder(Citation,size))) + 
pool_size <- ggplot(pools, aes(y = reorder(Pool,size), x = log(size)))  +
  #geom_vline(xintercept = 1135.71) +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  scale_x_continuous(expand=c(0,0)) +
  #geom_errorbar(ymin=min(log(size),ymax=max(log(size)))) +
  geom_errorbar(data=vegetation_pools,mapping=aes(y=Pool,x=log(size)),
                xmin=5,xmax=10,size=0.5,width=.25) +
  geom_point(data=this_study,mapping=aes(y=Pool,x=log(size)),size=7,pch=21,fill='white') + 
  ylab('') +
  #annotate("text", x=1450, y=2, label= "Average across studies") +
  xlab(bquote('Freshwater pool size'~(log(km^3)))) +
  theme(
    axis.text.x = element_text(color='black',size=13), #, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=19),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.15),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#add with bivariate VOD VWC relationship
#https://wilkelab.org/cowplot/articles/plot_grid.html

png(height = 2000,width=4000,res=300,
    'manuscript_figures/storage_multipanel.png')

print(plot_grid(vod_vwc_plot ,pool_size,
                labels = c('', ''),ncol = 2, nrow=1,
                rel_widths = c(2.5,2),
                rel_heights = c(1,1),label_size = 25))

dev.off()

#-------------------------------------------------------------------------------
# calculate total amount of water in aboveground vegetation ------



km_cubed_by_pixel <- aggregate(annual_storage ~
                                 lon + lat + group_2,get_km_cubed_3,
                               data=annual_turnover_lc)
head(km_cubed_by_pixel)

unique(km_cubed_by_pixel$group_2)

lc_total_pools <- aggregate(annual_storage~group_2,sum,data=km_cubed_by_pixel)
sum(lc_total_pools$annual_storage)
# about 33.3 %, a third, of water (136 cubic km) is stored in evergreen broadleaf forests
148.56/397.24


#quick plot of how total storage varies by land cover type
lc_pool_size <- ggplot(lc_total_pools, aes(y = reorder(group_2,annual_storage), x = annual_storage))  +
  #geom_vline(xintercept = 1135.71) +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  scale_x_continuous(expand=c(0,0),limits=c(0,154)) +
  ylab('') +
  #annotate("text", x=1450, y=2, label= "Average across studies") +
  xlab(bquote('Freshwater pool size'~(km^3))) +
  theme(
    axis.text.x = element_text(color='black',size=13), #, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=19),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.15),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

png(height = 2000,width=3000,res=300,
    'manuscript_figures/total_storage_land_cover.png')

print(lc_pool_size)

dev.off()

#-------------------------------------------------------------------------------
# ground-based transit compared to remote sensing transit broken up by season ------


library(FNN)
library(data.table)

#import and combine seasonal estimates of turnover
seasonal_turnover_import <- function(season){
  
  lc_id <- fread('./../../Data/land_cover_nsidc_ease2/land_cover_id.csv')  
  
  #set directories
  #season = 'winter'
  filepath <- paste0('./../../Data/turnover_from_python/seasonal/land_cover_csvs/',season,
                     '/')
  dir <- dir(filepath, full.names = T)
  
  #step 1 
  seasonal_turnover_summary_list <- list()
  for(i in dir[1:17]){
    
    #load in
    lc<-fread(i)
    
    if(season=='winter'){lc_filtered <- lc %>% #remove these in post, otherwise it screws things up for winter
      dplyr::filter(turnover > 0)}else{
        
        #filter out NA, inf, and zero values
        lc_filtered <- lc %>%
          dplyr::filter(lc01 != 'NA') %>% #only NAs for decidious needleleaf...
          dplyr::filter(turnover > 0) %>%
          dplyr::filter(turnover != 'Inf')}
    
    
    #get land cover ID
    name<-gsub(paste0('./../../Data/turnover_from_python/seasonal/land_cover_csvs/',
                      season,'//landclass.'),'', i)
    name<-gsub('.3856x1624.bin.nc.csv',
               '', name)
    lc_filtered$class_number <- as.integer(name)
    lc_filtered <- merge(lc_filtered,lc_id,by=c('class_number'))
    
    #store in list
    seasonal_turnover_summary_list[[i]] <- lc_filtered
    
  }
  
  
  seasonal_turnover <- do.call('rbind',seasonal_turnover_summary_list)
  rm(seasonal_turnover_summary_list)
  
  
  return(seasonal_turnover)
  
  
}


#import ground-based estimate
isotope <- read.csv('./../../Data/Isotopes/isotope_data.csv')
isotope <- isotope[-4,] #drop one with no coordinates

isotope <- isotope %>% 
  dplyr::filter(Drop.=='No',
                !season=='none')


seasons <- unique(isotope$season)
seasonal_turnover_comp <- list()
for(i in seasons){
  #i = 'summer'
  isotope_2 <- subset(isotope,season==i)
  
  #turn ground data to spdf
  coords_transit_ground <- isotope_2[ , c("Longitude", "Latitude"),]  # coordinates
  data_transit_ground   <- data.frame(isotope_2[c(7)])    # data
  crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords
  
  spdf_transit_ground <- SpatialPointsDataFrame(coords = coords_transit_ground,
                                                data = data_transit_ground, 
                                                proj4string = crs)
  
  
  seasonal_turnover <- seasonal_turnover_import(i)
  
  #turn vod raster data to spatial points DF
  turnover_2 <- seasonal_turnover %>%
    select(lon,lat,turnover,group,group_2)
  rownames(turnover_2) <- NULL
  
  # prepare coordinates, data, and proj4string
  coords_transit_vod <- turnover_2[ , c("lon", "lat")]   # coordinates
  data_transit_vod   <- turnover_2[ , 3:5]          # data
  crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs')  # proj4string of coords
  
  # make the SpatialPointsDataFrame object
  spdf_transit_vod <- SpatialPointsDataFrame(coords = coords_transit_vod,
                                             data = data_transit_vod, 
                                             proj4string = crs)
  
  
  
  #link ground-based coordinates to vod coordinates for storage
  nn1_transit = get.knnx(coordinates(spdf_transit_vod), coordinates(spdf_transit_ground), k=1)
  vector_transit <- data.frame(nn1_transit[1])
  vector_length <- as.numeric(nrow(vector_transit))
  # vector_transit <- vector_transit[c(1:vector_length),]
  spdf_vod_transit_df <- data.frame(spdf_transit_vod)
  
  column <- seq(1,1,1) #can change depending on length of k
  new_transit_df_list <- list()
  for(j in column){
    
    vector_transit_2 <- vector_transit[j]
    vector_transit_2 <- vector_transit_2[c(1:vector_length),]
    new_transit_df <- spdf_vod_transit_df[c(vector_transit_2),]
    new_transit_df$ID <- 1:nrow(new_transit_df)
    
    new_transit_df_list[[j]] <- new_transit_df
    
  }
  
  new_transit_df_list_df <- do.call('rbind',new_transit_df_list)
  new_transit_df_list_df <- aggregate(turnover~ ID + group,median,data=new_transit_df_list_df)
  
  
  new_transit_df_list_df <- new_transit_df_list_df %>%
    select(turnover,group,ID) %>%
    dplyr::filter(!group=='Urban') #in case this pops up
  
  #combine ground=based and vod-based water storage
  spdf_transit_ground_df <- data.frame(spdf_transit_ground)
  cbind_transit_ground_vod <- cbind(new_transit_df_list_df,spdf_transit_ground_df)
  cbind_transit_ground_vod <- cbind_transit_ground_vod %>%
    dplyr::filter(!group==c('Water'),
                  !group==c('Urban')) #don't want these two
  
  cbind_transit_ground_vod$season <- i
  
  seasonal_turnover_comp[[i]] <- cbind_transit_ground_vod
  
}

seasonal_turnover_comp_df <- do.call('rbind',seasonal_turnover_comp)
rm(seasonal_turnover_comp)


compare_transit <- ggplot(seasonal_turnover_comp_df,aes(x= turnover,
                              y=Mean.transit.time..days.,
                              color=group,shape=season)) +
  # scale_y_continuous(expand=c(0,0)) +
  # scale_x_continuous(expand=c(0,0)) +
  scale_color_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                              'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                     labels=c('Grassland'='Grassland','Forest'='Forest',
                              'Shrubland'='Shrubland','Savanna'='Savanna')) +
  geom_abline(slope=1,size=1,color='black') +
  geom_point(size=7,alpha=0.7) +
  xlab('Statellite-based transit (days)') +
  ylab('Istotope-based transit (days)') +
  annotate("text", x=11, y=9.5, label= "1:1 Line") +
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
    legend.position = c(0.5,0.3),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 2000,width=3000,res=300,
    'manuscript_figures/ground_satellite_transit_comparison.png')

print(compare_transit)

dev.off()



#-------------------------------------------------------------------------------
# storage versus transpiration across land cover types ------


head(annual_turnover_lc,1)
summary(annual_turnover_lc)

transp_storage_plot <- ggplot(annual_turnover_lc,aes(x=daily_transp_annual,
                                 y=annual_storage,
                                 color=group)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,20)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_color_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                              'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                     labels=c('Grassland'='Grassland','Forest'='Forest',
                              'Shrubland'='Shrubland','Savanna'='Savanna')) +
  geom_abline(slope=1,size=1,color='black') +
  geom_point(size=0.05,alpha=0.025) +
  stat_smooth(method='lm',se=F,size=1.25) +
  xlab('Canopy transpiration (mm/day)') +
  ylab(bquote('Aboveground water storage'~(mm/m^2))) +
  annotate("text", x=3, y=2.5, label= "1:1 Line") +
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
    legend.position = c(0.15,0.80),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 2000,width=3000,res=300,
    'manuscript_figures/storage_versus_transp.png')

print(transp_storage_plot)

dev.off()

#summary(lm(annual_storage~daily_transp_annual:group,data=annual_turnover_lc))


#-------------------------------------------------------------------------------