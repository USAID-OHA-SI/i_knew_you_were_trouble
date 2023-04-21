# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Pull site level MSD from PANO into MSD folder
# AUTHOR: Tim Essam | SI
# REF ID:   0062fc80
# LICENSE: MIT
# DATE: 2023-04-13
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(grabr)
    library(glamr)
  
  # Country list - pull from MSDs that have been stitched together
  # ou_list <- readRDS("Dataout/msd_site_dfs") %>% 
  #   distinct(operatingunit) %>% 
  #   pull()
  # remove(df)
  
  # Alt option
  ou_list <- c("Botswana", "Cameroon", "Cote d'Ivoire", "Eswatini", 
                "Ethiopia", "Lesotho", "Malawi","Mozambique", 
                "Rwanda", "South Africa", "South Sudan", "Uganda",
                "Zambia", "Zimbabwe")
  
  # Ref id
  ref_id <- "0062fc80"

# FETCH DAA DATA ============================================================================  
  # THANKS BK!
  
  accnt <- lazy_secrets(service = "datim")
  
  daa_df <- get_outable() %>% 
    filter(country %in% c(ou_list)) %>% 
    select(country, country_iso) %>% 
    pmap_dfr(~datim_orgunits(
      username = accnt$username,
      password = accnt$password,
      cntry = .x
    )) 

  # X walk does not work b/c the DAA files contain some orgunit_levels that are not present
  # in get_outable()
  
  # org_lvl_xwalk <- get_outable() %>% 
  #   tidyr::pivot_longer(cols = dplyr::ends_with("_lvl"),
  #                       names_to = "orgunit_label",
  #                       values_to = "orgunit_level") %>%
  #   dplyr::mutate(orgunit_label = stringr::str_remove(orgunit_label, "_lvl$"),
  #                 orgunit_level = as.character(orgunit_level))
  # 
  # 
  # tmp <- daa_df %>% 
  #   left_join(., org_lvl_xwalk, by = c("regionorcountry_code" = "country_iso", "orgunit_level" = "orgunit_level"))
  #   
  # 
  
  
  # Check the tally of orgunits at diff levels  
  daa_df %>% 
    count(regionorcountry_name, orgunit_level) %>% 
    spread(orgunit_level, n) %>% 
    arrange(desc(`7`), desc(`6`)) %>%
    gt::gt() %>% 
    gtExtras::gt_theme_nytimes() %>% 
    gt::sub_missing(missing_text = ".") %>% 
    gt::fmt_number(columns = where(is.numeric), 
                   decimals = 0) %>% 
    gt::cols_label(regionorcountry_name = "Operating Unit")
  
# MUNGE ============================================================================
  
  #  Keep all rows / orgunituids where the heirarchy mapping is not missing and is max value
  daa_df %>% 
    group_by(regionorcountry_name) %>% 
    mutate(max_oulvl = max(orgunit_level)) %>% 
    ungroup() %>% 
    filter(orgunit_level == max_oulvl) %>% 
    count(regionorcountry_name, orgunit_level) %>% 
    mutate(orgunit_level = "Facility Count") %>% 
    spread(orgunit_level, n) %>% 
    arrange(desc(`Facility Count`)) %>% 
    gt::gt() %>% 
    gtExtras::gt_theme_nytimes() %>% 
    gt::sub_missing(missing_text = ".") %>% 
    gt::fmt_number(columns = where(is.numeric), 
                   decimals = 0) %>% 
    gt::cols_label(regionorcountry_name = "Operating Unit") %>% 
    gt::tab_header(
      title = glue::glue("DAA FACILITY COUNTS BY OPERATING UNIT")
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("Source: DATIM DAA Files | Ref id: {ref_id}"))) %>% 
    gt::tab_options(
      source_notes.font.size = gt::px(10))
  
  # Keep the highest orgunit_level indicating facilities
  daa_fac_df <- 
    daa_df %>% 
    group_by(regionorcountry_name) %>% 
    mutate(max_oulvl = max(orgunit_level)) %>% 
    ungroup() %>% 
    filter(orgunit_level == max_oulvl)
    
    daa_fac_df %>% 
    count(regionorcountry_name, orgunit_level) 

# SPINDOWN ============================================================================

    daa_fac_df %>% write_rds(., file = "Dataout/daa_fac_df")
