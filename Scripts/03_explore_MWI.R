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
  
  # REF ID for plots
    ref_id <- "86b9e21f"
    
  # Functions  
  

# LOAD MALAWI DATA ============================================================================  
   
  # Load the MFL data and clean up names
  mwi_mfl <- read_sheet(ss_mwi, col_types = "c") %>% 
     janitor::clean_names() 
  
  mwi_mfl %>% distinct(code)  
  mwi_mfl %>% calc_share(., type) %>% janitor::adorn_totals() 
  
  # Load the DAA data with attributes
  mwi_daa <- read_sheet(ss_mwi_daa, col_types = "c")
  
  #Spread wider so we can join facility attribute information in
  mwi_daa_wide <- mwi_daa %>% 
    filter(dataelement %in% c("SA_FACILITY_TYPE", "SA_OWN_TYPE")) %>%
    pivot_wider(names_from = dataelement, values_from = value) 
 
   # What does the attribute information look like for the DAA file?
  mwi_daa_wide %>% calc_share(SA_FACILITY_TYPE)
  
  # Pull in site level data from AC datim API pull
  mwi_sites_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% "Malawi", sitetype == "Facility") %>% 
    mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy) %>% 
    separate(orgunit_hierarchy, sep = "/", into = c("org_first", "org_second", "org_third", "org_fourth"))
  
  # Keep in mind most USAID activities are above site -- so facility level data will not capture their reach
  mwi_nonsites_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% "Malawi", sitetype != "Facility") %>% 
    mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy) %>% 
    separate(orgunit_hierarchy, sep = "/", into = c("org_first", "org_second", "org_third", "org_fourth"))

# COMBINING MALAWI DATA ============================================================================
  
  # First Question to answer, what does each data set say about service delivery types? 
  # Let's combine the results from the MFL and datim DAA attributes data set to see
  
  mwi_mfl %>% 
    calc_share(type) %>% 
    gt_starter()
  
  mwi_daa_wide %>% 
    calc_share(SA_FACILITY_TYPE) %>% 
    gt_starter()
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

