# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Determine coverage of PEPFAR in relation to facilities offering PHC
# AUTHOR: Tim Essam | SI
# REF ID:   9c88567c
# LICENSE: MIT
# DATE: 2023-03-27
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
    library(readxl)
    
    
  # SI specific paths/functions  
    load_secrets()
    file_path <- "Data/HFR States data/"
    nga_states <- list.files(file_path, full.names = T)  
  
  # REF ID for plots
    ref_id <- "9c88567c"
    
  # Functions  
  # Set up column types for each State facilities
    text_type <- c("text", "text", "text", "text", "text",
                   "text", "text", "text", "text", "text",
                   "text", "text", "text", "numeric", "numeric",
                   "text", "text", "text", "text", "text")

# LOAD DATA ============================================================================  

  # nga_states %>% 
  #     setNames(., sub("\\.xlsx$", "", basename(.))) %>% 
  #     map(read_excel(.x, col_types = c("text", "text", "text", "numeric", "text",
  #                                  "text", "text", "date")))
    
  # What column types do we need to specify
    nga_states[1] %>% read_excel(.) %>% glimpse()
    
  # Check how many rows we should have
  # 39,467
   df_check <- map(nga_states, ~read_excel(.x, col_types = text_type)) 
   map(df_check, ~dim(.x)[1]) %>% unlist() %>% sum()
 
 # Check how many we get when we row_bind
   df_nga <-  map_dfr(nga_states, ~read_excel(.x, col_types = text_type)) 
 
# MUNGE ============================================================================

  # How many distinct facilities?
   df_nga %>% count(facility_code, facility_name) %>% filter(n > 1)

  # how many PHC facilities?
   df_nga %>% count(facility_level) %>% 
     mutate(share = n / sum(n))
       
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

