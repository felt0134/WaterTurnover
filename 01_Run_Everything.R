

#clear work environment and set up libraries and functions
rm(list=ls())

#load packages
pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr','maptools','data.table',
          'rstudioapi','ggplot2','tidyverse')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Load in-house functions

# Need split up turnover by seasons (4-month chunks) so we can make 4-panel
# figure showing tunover by seasons across the globe


source('02_Functions.R')
