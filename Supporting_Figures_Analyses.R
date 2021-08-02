
# supporting analyses and figures

# per-pixel sample size for transit time ------



transit_sample_size <- raster('./../../../Data/Derived_Data/Sample_Sizes/VWC_global_transit_sample_size.tif')

png('Figures/Supporting/transit_time_sample_size.png',
    width=8,height=6,units="in",res=400)
plot(transit_sample_size,main='Sample Size (Number of months)')

dev.off()