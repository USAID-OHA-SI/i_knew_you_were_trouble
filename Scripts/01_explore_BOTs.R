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
  library(googlesheets4)
  library(googledrive)
  library(gt)
  library(gtExtras)

  # SI specific paths/functions  
  source("Scripts/00_helper_functions.R")
  load_secrets()

  # Botswana Data Sources #
  # MFL
  ss_bts <- "1PgZZ4J1Nx_OA5EC6vxbLcw5xJ6iDp2DvPvqhe258o8c"
  
  # Sites pulled via API by AC
  ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
  
  # SS DAA data with attributes from Jason K. (SGAC)
  ss_bts_daa <- "1aBcZRV-Wwk4_RpfzQtIuLw96-1CtpBQMKPOJaKUBfkk"
  
  # Use google drive to download file and then pass temp path through to vroom to open
  temp <- tempfile(fileext = ".zip")
  dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
  
  # MSD site file to check attributes
  bts_msd_site_path  <- return_latest(folder = "Data", pattern = "MER|Botswana")

  # Data Exchange Organisation Unit file (DAA file sans attributes)   
  bts_deou <- "Data/BTS_DEOU.csv"
  
  # Other Setup Options #
  
 # Grab metadata
  get_metadata(bts_msd_site_path)

 # REF ID for plots
  ref_id <- "fbd7d896"

 # Functions & Objects
  cntry_list <- c("Botswana")
  
  calc_share <- function(df, sdt_var){
    df %>% 
      count({{sdt_var}}, sort = T) %>% 
      mutate(Share = n / sum(n))
  }


# Botswana Data Assessment ============================================================================  

  # Load the MFL data and clean up names
  bts_mfl <- read_sheet(ss_bts, skip = 2, col_types = "c") %>% janitor::clean_names() %>% 
    mutate(facility_name = str_to_title(facility_name))
  
  # What does the attribute information look like for MLF?
    bts_mfl %>% calc_share(., service_delivery_type) 
  
  # Load in the new DAA data with facility attribute information
  bts_daa <- read_sheet(ss_bts_daa, col_types = "c")
  
  #Spread wider so we can join facility attribute information in
  bts_daa_wide <- bts_daa %>% 
    filter(dataelement %in% c("SA_FACILITY_TYPE", "SA_OWN_TYPE")) %>%
    pivot_wider(names_from = dataelement, values_from = value) 
  
  # What does the attribute information look like for the DAA file?
  bts_daa_wide %>% calc_share(SA_FACILITY_TYPE)
  
  # How many distinct sites are in MSD? 406
  bts_mer_sites <- read_psd(bts_msd_site_path) %>%
    filter(sitetype == "Facility", fiscal_year %in% c(2022, 2023)) %>% 
    distinct(orgunituid, facilityuid, facility, sitename)
  
  bts_mer_nonsites <- read_psd(bts_msd_site_path) %>% 
    filter(sitetype != "Facility", fiscal_year %in% c(2022, 2023)) %>% 
    distinct(orgunituid, sitename)
  
  # Notes - "[^\/]+$" matches the non-slashes substring right at the end of what you test
  bts_sites_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% "Botswana", sitetype == "Facility") %>% 
    mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy) %>% 
    separate(orgunit_hierarchy, sep = "/", into = c("org_first", "org_second", "org_third", "org_fourth"))
  
  # Keep in mind most USAID activities are above site -- so facility level data will not capture their reach
  bts_nonsites_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% "Botswana", sitetype != "Facility") %>% 
    mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy) %>% 
    separate(orgunit_hierarchy, sep = "/", into = c("org_first", "org_second", "org_third", "org_fourth"))
  
# Combining Botswana Data -------------------------------------------------

  # First Question to answer, what does each data set say about service delivery types? 
  # Let's combine the results from the MFL and datim DAA attributes data set to see
  bts_mfl %>% calc_share(., service_delivery_type) %>% 
    gt() %>% 
    fmt_percent(columns = Share, 
                decimals = 0) %>% 
    cols_label(service_delivery_type = "Attribute", 
               n = "Count") %>% 
    grand_summary_rows(columns = 2:3,
                       fns = list(
                         Total = ~sum(.))
                       ) %>% 
    tab_header(
      title = "Botswana Master Facility List Attribute Summary"
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: Master Facility List provided by SI staff"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() %>% 
    gtsave_extra( filename = "Images/BTS_MFL_summary.png")
  
  
  bts_daa_wide %>% calc_share(SA_FACILITY_TYPE) %>% 
    gt() %>% 
    fmt_percent(columns = Share, 
                decimals = 0) %>% 
    cols_label(SA_FACILITY_TYPE = "Attribute", 
               n = "Count") %>% 
    grand_summary_rows(columns = 2:3,
                       fns = list(
                         Total = ~sum(.)
                         )
                       ) %>% 
    tab_header(
      title = "Botswana DATIM DAA Attribute Summary"
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM Data Alignment Activity Attribute Data 2023"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() %>% 
    gtsave_extra( filename = "Images/BTS_DAA_attribute_summary.png")
  
  
  # What happens if we join the api site data with the attribute data provided by Jason K.
    
  
  
    bts_sites_datim <- tidylog::left_join(bts_daa_wide, bts_sites_api, by = c("orgunit" = "orgunituid"))

  # what share of PHC does PEPFAR operate in?
    bts_sites_datim %>% 
      filter(indicatortype == "DSD") %>% 
      count(SA_FACILITY_TYPE, indicatortype) %>% spread(indicatortype, n) %>% 
      janitor::adorn_totals()
  
    
  
  # If we were to try a "fuzzy join" on hte facilty names with Botswana
    # Not viable -- too many mis-matches
  tmp <- tidylog::left_join(bts_mfl, bts_mer_sites, by = c("facility_name" = "sitename"))
  tmp %>% select(facility_name, facility) %>% View()

# Malawi ============================================================================

  # Facilities from DATIM - 
  # Flag all rows with DELETE in orgunit_name as a drop row
  datim_bwa <- datim_bwa %>% 
    mutate(drop_flag = ifelse(str_detect(orgunit_name, "DELETE"), "drop", "keep")) %>% 
    filter(drop_flag == "keep")
  
  
  # Check for duplicate facility names
  mfl_bwa %>% group_by(facility_name) %>% tally() %>% filter(n > 1)
  
  sites_bwa <- sites %>% filter(operatingunit == "Botswana") %>% 
    filter(sitetype == "Facility") %>% distinct(facilityname, org_third) %>% 
    mutate(district = str_squish(org_third) %>% str_replace_all(., " District", ""))
  
  comparr_var_levels
  setdiff(unique(mfl_bwa$district), unique(sites_bwa$district))
  setdiff(unique(sites_bwa$district), unique(mfl_bwa$district))

  
  tmp <- mfl_bwa %>% tidylog::left_join(., sites_bwa, by = c("facility_name" ="facilityname"))

# VIZ ============================================================================
  
  purrr::map_dfr(setNames(rds_list, rds_list), readRDS, .id = "file")
  
  
#  

# SPINDOWN ============================================================================

