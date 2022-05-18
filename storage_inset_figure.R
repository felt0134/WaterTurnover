

#make storage figure with vegetation pools as an inset

library(grid)

lc_pool_size_inset <- ggplot(lc_total_pools, aes(x = reorder(group_2,annual_storage), y = annual_storage))  +
  #geom_vline(xintercept = 1135.71) +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  scale_y_continuous(expand=c(0,0),limits=c(0,154)) +
  xlab('') +
  #annotate("text", x=1450, y=2, label= "Average across studies") +
  ylab(bquote('Pool size'~(km^3))) +
  theme(
    axis.text.x = element_text(color='black',size=8, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=7),
    axis.title.x = element_text(color='black',size=8),
    axis.title.y = element_text(color='black',size=10),
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


main_panels <- plot_grid(vod_vwc_plot ,pool_size,
                         labels = c('', ''),ncol = 2, nrow=1,
                         rel_widths = c(1,1.35),
                         rel_heights = c(1,1),label_size = 25)

vp <- viewport(width = 0.2, height = 0.4, x = 0.9,y=0.40)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(main_panels)
  print(lc_pool_size_inset , vp = vp)
}



png(height = 2000,width=4700,res=300,
    'manuscript_figures/storage_multipanel_inset.png')

full()

dev.off()


