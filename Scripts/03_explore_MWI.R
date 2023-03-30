# PROJECT: Analysis of Site level data on PHC in Malawi
# PURPOSE: Determine coverage of PEPFAR in relation to facilities offering PHC
# AUTHOR: Tim Essam | SI
# REF ID:   fbd7d896
# LICENSE: MIT
# DATE: 2023-03-23
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

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
    library(googlesheets4)
    library(googledrive)
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
  source("Scripts/00_helper_functions.R")
  load_secrets()

  # Botswana Data Sources #
  # MFL
  ss_mwi <- "1bTTWTdenQHVCGX0U-12OwjGGzt8aKQ_NRC8d8PjJY9A"
  
  # Sites pulled via API by AC
  ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
  
  # SS DAA data with attributes from Jason K. (SGAC)
  ss_mwi_daa <- "1HFQNFuJ1fJay9BX-jt2eWB5viQBjIZ4GR9iVe66Iwbk"
  
  # Use google drive to download file and then pass temp path through to vroom to open
  temp <- tempfile(fileext = ".zip")
  dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "86b9e21f"
    
  # Functions  
  

# LOAD MALAWI DATA ============================================================================  
   
  # Load the MFL data and clean up names
   mwi_mfl <- read_sheet(ss_mwi, col_types = "c") %>% 
      janitor::clean_names() 
  
  mwi_mfl %>% distinct(code)  
  mwi_mfl %>% calc_share(., type) %>% janitor::adorn_totals() %>% gt()
    
    

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

