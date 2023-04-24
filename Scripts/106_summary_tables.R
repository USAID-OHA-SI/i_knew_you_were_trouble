# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  summary table across OUs
# REF ID:   bc27f98d 
# LICENSE:  MIT
# DATE:     2023-04-20
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
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

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "bc27f98d"

# IMPORT ------------------------------------------------------------------
  
  # Sites pulled via API by AC
  ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
  # Use google drive to download file and then pass temp path through to vroom to open
  temp <- tempfile(fileext = ".zip")
  dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
  
  # Notes - "[^\/]+$" matches the non-slashes substring right at the end of what you test
  # Contains all site types including community  
  datim_sites_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% ou_list) %>% 
    mutate(facilityname = str_extract(orgunit_hierarchy, "([^\\/]+$)"), .after = orgunit_hierarchy)
  
  #attribute data from 103_download_daa_deou_facilities
  df_daa <- readRDS("Dataout/daa_fac_df") 

# MUNGE -------------------------------------------------------------------

  #prep API data - filter to facility sites and pivot indicator type
    api_tagged <- datim_sites_api %>% 
      select(1:6) %>% 
      filter(sitetype == "Facility") %>% 
      mutate(value = 1) %>% 
      pivot_wider(names_from = indicatortype, values_from = value) %>% 
      mutate(pepfar_fp = case_when(
        is.na(DSD) & is.na(TA) ~ "Non-PEPFAR",
        TRUE ~ "PEPFAR"
      )) %>% 
      arrange(pepfar_fp) %>% 
      rename(sitename = facilityname) 
    
  #join DATIM API data to DAA (verify that this is the correct way to join)
    df_gt_all <- daa_df %>% 
      tidylog::left_join(api_tagged, by = c("orgunit_internal_id" = "orgunituid")) %>% 
      mutate(merge_status_two = ifelse(is.na(sitename), "non-PEPFAR", "PEPFAR")) %>% 
      rename(orgunituid= orgunit_parent_internal_id)
    
# VIZ -----------------------------------------------------------------------------------
    
  #prep data for summary #1: PEPFAR share of health facilities by OU 
  df_gt_viz <-  df_gt_all %>% 
      mutate(indicator_type = case_when(DSD ==1 & TA ==1 ~ "DSD & TA",
                                        DSD == 1 & is.na(TA) ~ "DSD",
                                        TA == 1 & is.na(DSD) ~ "TA",
                                       # is.na(DSD) & is.na(TA) & merge_status_two == "PEPFAR" ~ "No Support Type",
                                        TRUE ~ "Not PEPFAR Supported")) %>% 
      distinct(regionorcountry_name, orgunit_internal_id, merge_status_two, indicator_type) %>%
      count(regionorcountry_name, indicator_type, sort = T) %>% 
      group_by(regionorcountry_name) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      pivot_wider(names_from = "indicator_type", values_from  = "n") %>% 
      mutate(`DSD & TA` = ifelse(is.na(`DSD & TA`), 0, `DSD & TA`),
             TA = ifelse(is.na(TA), 0, TA)) %>% 
      mutate(`PEPFAR Share` = (DSD + TA + `DSD & TA`)/total,
             `non-PEPFAR Share` = `Not PEPFAR Supported`/total) %>% 
      relocate(`Not PEPFAR Supported`, .after = `PEPFAR Share`) %>% 
      relocate(TA, .after = DSD) %>% 
      arrange(desc(`PEPFAR Share`)) %>% 
      mutate(`PEPFAR Share` = percent(`PEPFAR Share`, 1),
             `non-PEPFAR Share` = percent(`non-PEPFAR Share`, 1)) %>% 
      rename(Country = regionorcountry_name)
    
    #Summary table #1: PEPFAR share of health facilities by OU 
    df_gt_viz %>% 
      gt() %>% 
      fmt_number(columns = c(2,3,4,5,7), 
                 decimals = 0) %>% 
      cols_hide(total) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = c("left"), 
            color = trolley_grey_light,
            weight = px(2)
          )
        ),
        locations = list(
          cells_body(
            columns = c(3, 7)
          )
        )
      ) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      gt_theme_nytimes() %>% 
      tab_header(
        title = glue("SHARE OF HEALTH FACILITIES SUPPORTED BY PEPFAR BY OU")) %>%
      tab_source_note(
        source_note = gt::md(glue("Source: DATIM Data Alignment Activity Attribute Data & DATIM API 2023"))) %>%
      tab_options(
        source_notes.font.size = px(10)) %>%
      gt_highlight_cols(
        columns = c(`PEPFAR Share`,`non-PEPFAR Share`),
        fill = grey10k,
        font_weight = 400,
        alpha = 0.45
      ) %>% 
      gtsave_extra(filename = glue("Images/PEPFAR_SUPPORT_BY_OU.png"))
    
  
