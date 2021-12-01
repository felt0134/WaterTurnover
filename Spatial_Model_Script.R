# script to run model and save output

#things to vary in other script
#climate_variable
#climate_list
#turnover_scale



veg_list <-c('grassland','forest','tundra','shrubland','cropland')

slopes_veg_annual_list<- list()
r.squared_veg_annual_list <- list()
look <- climate_list$aridity
unique(look$cover)

for(i in veg_list){
  
  #aridity =1
  #pet = 2
  #map=3
  
  #aridity
  
  #run the function
  test_function <- get_climate_model_coefs(list=climate_list,x=climate_variable,veg=i)
  
  #get slopes
  slopes_df <- data.frame(do.call('rbind',test_function[[1]]))
  slopes_df$climate_mean <- as.numeric(slopes_df$climate_mean)
  
  #get r.squared
  r.squared_df <- data.frame(do.call('rbind',test_function[[2]]))
  r.squared_df$summary.transit_lm..r.squared <- as.numeric(r.squared_df$summary.transit_lm..r.squared)
  
  slopes_veg_annual_list[[i]] <- slopes_df
  r.squared_veg_annual_list[[i]] <- r.squared_df
  

  
  #hist(slopes_df$climate_mean)
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
write.csv(slopes_veg_annual_df,paste0('./../../../Data/Derived_data/Model_outputs/',turnover_scale,'_coef_',climate_name,'.csv'))
write.csv(r.squared_veg_annual_df,paste0('./../../../Data/Derived_data/Model_outputs/',turnover_scale,'_r.squared_',climate_name,'.csv'))


ggplot(slopes_veg_annual_df,aes(climate_mean,color=veg)) +
  geom_density()

