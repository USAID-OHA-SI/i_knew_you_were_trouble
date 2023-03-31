# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Determine coverage of PEPFAR in relation to facilities offering PHC
# AUTHOR: Tim Essam | SI
# REF ID:   fbd7d896
# LICENSE: MIT
# DATE: 2023-03-23
# NOTES: Tim Essam | SI

 
# LOCALS & SETUP FOR QUARTO========================================================================

# Libraries
library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(googledrive)
library(googlesheets4)
library(gtExtras)
library(gt)

# SI specific paths/functions  
load_secrets()

# Sites pulled via API by AC
# ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
# Use google drive to download file and then pass temp path through to vroom to open
# temp <- tempfile(fileext = ".zip")
# dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)

# MSD site file to check attributes


# README: IMPORTANT PATH CHOICE
# The folder path has to be removed 1 level because the Quarto file lives in the Scripts folder
bts_msd_site_path  <- return_latest(folder = "../Data", pattern = "Site_IM.*Botswana")
mwi_msd_site_path <- return_latest(folder = "../Data", pattern = "Site_IM.*Malawi")

# SS DAA data with attributes from Jason K. (SGAC)
ss_bts_daa <- "1aBcZRV-Wwk4_RpfzQtIuLw96-1CtpBQMKPOJaKUBfkk"
ss_mwi_daa <- "1HFQNFuJ1fJay9BX-jt2eWB5viQBjIZ4GR9iVe66Iwbk"

# DATIM Orgunit Datasets (Denominators)
ss_bts_deou <- "17BM2TUhMqKao8UrppAXJsbf8juj6lL8T4m8hAO7PKqA"
ss_mwi_deou <- "1tUXDzrXDDFHKxz9kI2qQjoPMFoRk7l0J2rFUzcUviOg"

# REF ID for plots
ref_id <- "3a06b30d"

# metadata
get_metadata(bts_msd_site_path)

# Functions  
source("00_helper_functions.R")
cntry_list <- c("Botswana", "Malawi")


# SPINDOWN ============================================================================

