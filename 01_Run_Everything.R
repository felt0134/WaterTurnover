# run all scripts from source

rm(list=ls())

#install packages
# pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr',
#           'BIOMASS','maptools','data.table','rstudioapi','taxize','ggplot2')
# lapply(pkgs, install.packages, character.only = TRUE) 

#load packages
pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr',
          'BIOMASS','maptools','data.table','rstudioapi','taxize','ggplot2')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Load in-house functions
source('02_Functions.R')

# Generate derived data sets - do only once:

#source('03_Generate_Data.R')
#source('04_Generate_Storage_Transp_Data.R')

# Import data
source('05_Import_Storage_Transp_Data.R')

# Summarize data
source('06_Summarize_Turnover_Data.R')

# to do:


# Need split up turnover by seasons (4-month chunks) so we can make 4-panel
# figure showing tunover by seasons across the globe


