# run all scripts from source

rm(list=ls())

#install packages
# pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr',
#           'BIOMASS','maptools','data.table','rstudioapi')
# lapply(pkgs, install.packages, character.only = TRUE) 

#load packages
pkgs <- c("ncdf4", "raster", "rgdal", "reshape2", "sp",'dplyr',
          'BIOMASS','maptools','data.table','rstudioapi')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Load in-house functions
source('02_Functions.R')