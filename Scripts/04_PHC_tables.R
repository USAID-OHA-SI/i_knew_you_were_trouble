# PROJECT: Analysis of Site level data on PHC
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
    library(googledrive)
    library(googlesheets4)
    library(gtExtras)
    library(gt)
    
  # SI specific paths/functions  
    load_secrets()
      
  # Sites pulled via API by AC
    ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
  # Use google drive to download file and then pass temp path through to vroom to open
    temp <- tempfile(fileext = ".zip")
    dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
    
  # MSD site file to check attributes
    bts_msd_site_path  <- return_latest(folder = "Data", pattern = "MER|Botswana")
  
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
    source("Scripts/00_helper_functions.R")
    cntry_list <- c("Botswana", "Malawi")

# LOAD DATA ============================================================================  

  # Site data from AC -- facilty name is not unique, it's stacked by service type
  # Notes - "[^\/]+$" matches the non-slashes substring right at the end of what you test
  # Contains all site types including community  
   datim_sites_api <- vroom::vroom(dl$local_path) %>% 
      filter(operatingunit %in% cntry_list) %>% 
      mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy) %>% 
      separate(orgunit_hierarchy, sep = "/", into = c("org_first", "org_second", "org_third", "org_fourth"))
    
  # Botswana data assets
    bts_daa <- read_sheet(ss_bts_daa, col_types = "c") %>% 
      pivot_daa()
    bts_deou <- read_sheet(ss_bts_deou, col_types = "c")  
    bts_msd_site <- read_psd(bts_msd_site_path) %>% 
      munge_msd()
    
    
    bts_msd_site %>% 
      filter(fiscal_year == metadata$curr_fy, standardizeddisaggregate == "Total Numerator",
             indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS")) %>% 
      group_by(fiscal_year, indicator) %>% 
      summarize(across(c(cumulative), \(x) sum(x, na.rm = T)))
    
  # Malawi data assets
    mwi_daa <- read_sheet(ss_mwi_daa, col_types = "c") %>% 
      pivot_daa()
    mwi_deou <- read_sheet(ss_mwi_deou, col_types = "c")
    mwi_msd_site <- read_psd(mwi_msd_site_path) %>% 
      munge_msd()

    
# CREATE MERGED DATASETS ============================================================================
  
  #  First, let's create the attribute merge so we are working with the right unit of analysis
    map(list(bts_deou, bts_daa, datim_sites_api), ~names(.x))
    
  # 132 facilities in the DEOU that are not in the DAA attribute data; 407 NOT in PEPFAR data
  # 407 facilities in the DEOU that are not in site API pull
    bts_sites <- tidylog::left_join(bts_deou, bts_daa, by = c("orgunit_internal_id" = "orgunit")) %>% 
      rename(orgunituid = orgunit_internal_id) %>% 
      mutate(merge_status = ifelse(is.na(period), "deou_only", "merged")) %>% 
      tidylog::left_join(., datim_sites_api %>% filter(operatingunit == "Botswana"), by = c("orgunituid" = "orgunituid")) %>% 
      mutate(merge_status_two = ifelse(is.na(facilityname), "non-PEPFAR", "PEPFAR"))
    
  # How many sites does PEPFAR work in that have attribute information?
 bts_sites %>% 
    distinct(orgunituid, merge_status, merge_status_two) %>% 
    count(merge_status, merge_status_two) %>% 
    spread(merge_status, n)
    
    
    
  # keep merged + PEPFAR results
    bts_sites %>% 
      filter(merge_status == "merged") %>% 
      distinct(orgunituid, merge_status_two, SA_FACILITY_TYPE) %>% 
      calc_share(merge_status_two, SA_FACILITY_TYPE) %>% 
      mutate(fac_type = fct_reorder(SA_FACILITY_TYPE, Share, .desc = T)) %>% 
      arrange(fac_type, merge_status_two, Share) %>% 
      gt_starter(., c(n, Share)) %>% 
      tab_header(
        title = "BOTSWANA PEPFAR COVERAGE SUMMARY BY FACILITY TYPE"
      ) %>% 
      tab_source_note(
        source_note = gt::md(glue("Source: DATIM Data Alignment Activity Attribute Data 2023"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      cols_hide(fac_type) %>% 
      cols_label(SA_FACILITY_TYPE = "Facility Type", 
                 merge_status_two = "COVERAGE")

    
    # Now bring in the site level indicator data
    bts_sites_mer <- tidylog::left_join(bts_deou, bts_daa, by = c("orgunit_internal_id" = "orgunit")) %>% 
      rename(orgunituid = orgunit_internal_id) %>% 
      mutate(merge_status = ifelse(is.na(period), "deou_only", "merged")) %>% 
      tidylog::left_join(., bts_msd_site) %>% 
      mutate(merge_status_two = ifelse(is.na(sitename), "non-PEPFAR", "PEPFAR"))
    
    # Summary table of indicators by facility type for Q1
    bts_sites_mer %>% 
      filter(merge_status_two == "PEPFAR") %>% 
      group_by(SA_FACILITY_TYPE) %>% 
      summarize(across(c(HTS_TST_POS, TX_NEW, TX_CURR), \(x) sum(x, na.rm = T))) %>% 
      ungroup() %>% 
      mutate(across(c(HTS_TST_POS, TX_NEW, TX_CURR), \(x) x / sum(x, na.rm = T), .names = "{.col}_share")) %>% 
      janitor::adorn_totals()


    
  # 132 facilities in the DEOU dataset that are not included in bts_daa (697 match)
    
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

