# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Analyze zip file of site attribute data from select countries
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
    library(gt)
    library(gtExtras)
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    zip_path <- "Data/Site Attrib Data/"
    
    site_attrib_ous <- list.files(path = zip_path, full.names = T)
  
  # Extract names to use in 
    site_attrib_names <- sub(" data.*", "", list.files(path = zip_path))
        
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "7853bd3e"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  # Read in the the files, create a new column to track the OU and set the list 
  # names for identification  
  site_attrib_df <- map2(.x = site_attrib_ous,
                         .y = site_attrib_names,
                         ~vroom::vroom(.x) %>% mutate(operatingunit = .y)) 
  names(site_attrib_df) <- set_names(site_attrib_names)  
    
  site_attrb_tbl <- map_dfr(site_attrib_df, ~.x %>% 
        filter(dataelement == "SA_FACILITY_TYPE") %>% 
        count(value, operatingunit) %>% arrange(desc(n)) %>% 
          spread(value, n))
  
  # Create a GT summary table
  site_attrb_tbl %>% 
    rowwise() %>% 
    mutate(total = sum(c_across(2:9), na.rm = T)) %>%
    mutate(operatingunit = ifelse(operatingunit == "CI", "Cote d'Ivoire", operatingunit)) %>% 
    gt() %>% 
    sub_missing() %>% 
    fmt_number(columns = is.numeric, 
               decimal = 0) %>% 
    gt_theme_nytimes() %>% 
    tab_header(
      title = glue("DAA FACILITY TYPES BY OPERATING UNITS")
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM DAA Dataset | Ref id: {ref_id}"))) %>% 
    tab_options(
      source_notes.font.size = px(10))
           

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

