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
    source("Scripts/00_helper_functions.R")
  
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
  
  #attribute data from 103_download_daa_deou_facilities
  df_daa_sa <- readRDS("Dataout/site_attrib_df") 
  
  

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
    
  #join DATIM API data to DAA and site attributes (verify that this is the correct way to join)
    df_gt_all <- df_daa %>% 
    tidylog::left_join(df_daa_sa, by = c("orgunit_internal_id" = "orgunit")) %>% 
    mutate(merge_attributes = ifelse(is.na(dataelement), "DAA only", "DAA & Attrb")) %>% 
    tidylog::left_join(api_tagged, by = c("orgunit_internal_id" = "orgunituid")) %>% 
    mutate(merge_api = ifelse(is.na(sitetype), "DAA only", "DAA & api")) %>% 
      mutate(merge_status_two = ifelse(is.na(sitename), "non-PEPFAR", "PEPFAR")) %>% 
      rename(orgunituid= orgunit_parent_internal_id)
    
# SUMMARY #1:  PEPFAR share of health facilities by OU --------------------------------------
    
  #prep data for summary #1: PEPFAR share of health facilities by OU 
  df_gt_viz1 <-  df_gt_all %>% 
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
      replace(is.na(.), 0) %>% 
      mutate(`PEPFAR Share` = (DSD + TA + `DSD & TA`)/total,
             `non-PEPFAR Share` = `Not PEPFAR Supported`/total) %>% 
      relocate(`Not PEPFAR Supported`, .after = `PEPFAR Share`) %>% 
      relocate(TA, .after = DSD) %>% 
      arrange(desc(`PEPFAR Share`)) %>% 
      mutate(`PEPFAR Share` = percent(`PEPFAR Share`, 1),
             `non-PEPFAR Share` = percent(`non-PEPFAR Share`, 1)) %>% 
      rename(Country = regionorcountry_name) %>% 
      mutate(TA = ifelse(TA == 0, NA, TA),
             `DSD & TA` = ifelse(`DSD & TA` == 0, NA, `DSD & TA`))
    
    #Summary table #1: PEPFAR share of health facilities by OU 
    df_gt_viz1 %>% 
      gt() %>% 
      sub_missing(missing_text = ".",
      ) %>%  
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
        source_notes.font.size = px(10),
        column_labels.font.size = px(15)) %>%
      gt_highlight_cols(
        columns = c(`PEPFAR Share`,`non-PEPFAR Share`),
        fill = grey10k,
        font_weight = 400,
        alpha = 0.45
      ) %>% 
      gtsave_extra(filename = glue("Images/PEPFAR_SUPPORT_BY_OU2.png"))
    
    
# SUMMARY #2: % Health center/posts out of total facilities in the country  byOU ----------------
    
    
    ou_totals_daa <- df_gt_all %>% 
      mutate(indicator_type = case_when(DSD ==1 & TA ==1 ~ "DSD & TA",
                                        DSD == 1 & is.na(TA) ~ "DSD",
                                        TA == 1 & is.na(DSD) ~ "TA",
                                        # is.na(DSD) & is.na(TA) & merge_status_two == "PEPFAR" ~ "No Support Type",
                                        TRUE ~ "Not PEPFAR Supported")) %>% 
      distinct(regionorcountry_name, orgunit_internal_id, merge_status_two, indicator_type) %>%
      count(regionorcountry_name, indicator_type, sort = T) %>% 
      group_by(regionorcountry_name) %>%
      mutate(total_sites = sum(n)) %>% 
      ungroup() %>% 
      select(regionorcountry_name, total_sites) %>% 
      distinct()


    df_gt_viz2 <- df_gt_all %>% 
     # pivot_daa() %>% 
      filter(dataelement == "SA_FACILITY_TYPE") %>%
    #  mutate(fac_type = collapse_2())
      collapse_fac_type(., unique_var = value) %>%
      distinct(regionorcountry_name, orgunit_internal_id, fac_type) %>%
      count(regionorcountry_name, fac_type, sort = T) %>% 
      group_by(regionorcountry_name) %>%
      mutate(total_attr = sum(n)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = "fac_type", values_from  = "n") %>% 
      select(regionorcountry_name, `Primary Health Center`, `Health Post`, `Mobile Health Clinic`) %>% 
      # relocate(Hospital, .after = `Health Post`) %>% 
      # relocate(`Other Facility`, .after = `Mobile Health Clinic`) %>% 
      left_join(ou_totals_daa) %>% 
      replace(is.na(.), 0) %>% 
      mutate(phc_share = `Primary Health Center`/total_sites,
             health_post_share = `Health Post` / total_sites,
             mobile_share = `Mobile Health Clinic`/total_sites) %>% 
      arrange(desc(phc_share)) %>% 
      # mutate(phc_share = percent(phc_share, 1),
      #        health_post_share = percent(health_post_share, 1),
      #        mobile_share = percent(mobile_share, 1)) %>% 
      relocate(phc_share, .after = `Primary Health Center`) %>% 
      relocate(health_post_share, .after = `Health Post`) %>% 
      relocate(mobile_share, .after = `Mobile Health Clinic`) %>% 
      rename(Country = regionorcountry_name) %>% 
      mutate(`Health Post` = ifelse(`Health Post` == 0, NA, `Health Post`),
             `Mobile Health Clinic` = ifelse(`Mobile Health Clinic` == 0, NA, `Mobile Health Clinic`))
    
    df_gt_viz2 %>%
      summary_tbl_ou_starter(viz_type = "02_share_over_total") %>% 
      cols_label(phc_share = "(share)",
                 health_post_share = "", 
                 mobile_share = "", 
                 total_sites = "Total number of facilities") %>% 
      tab_header(
        title = glue("SHARE OF PRIMARY HEALTH CENTERS & HEALTH POSTS BY TOTAL FACILITIES WITHIN OU")
      ) %>% 
      gt_color_rows(columns = c(2,4,6), na.color = "white", 
                    palette = c("#f7f7f7", scooter_med)) %>% 
      gtsave_extra(gt_fac_by_total_site, filename = glue("Images/TABLE_2_FAC_BY_TOTAL_SITE.png"))  
    
  # SUMMARY #3 -------------------------------------------------------------------
    
    df_gt_viz3 <- df_gt_all %>% 
      # pivot_daa() %>% 
      filter(dataelement == "SA_FACILITY_TYPE") %>%
      #  mutate(fac_type = collapse_2())
      collapse_fac_type(., unique_var = value) %>%
      distinct(regionorcountry_name, orgunit_internal_id, merge_status_two, fac_type) %>%
      count(regionorcountry_name, merge_status_two, fac_type, sort = T) %>% 
      pivot_wider(names_from = "fac_type", values_from  = "n") %>%
      select(regionorcountry_name,merge_status_two,`Primary Health Center`, `Health Post`) %>% 
      replace(is.na(.), 0) %>% 
      group_by(regionorcountry_name) %>% 
      mutate(total_phc = sum(`Primary Health Center`),
             total_hp = sum(`Health Post`),
             phc_share = `Primary Health Center`/total_phc,
             health_post_share = `Health Post`/total_hp) %>% 
    ungroup() %>% 
      filter(merge_status_two == "PEPFAR") %>% 
      relocate(total_phc, .after= `Primary Health Center`) %>% 
      relocate(phc_share, .after = total_phc) %>% 
      arrange(desc(phc_share)) %>% 
      # mutate(phc_share = percent(phc_share, 1),
      #        health_post_share = percent(health_post_share, 1)) %>% 
    rename(Country = regionorcountry_name) %>% 
      mutate(`Health Post` = ifelse(`Health Post` == 0, NA, `Health Post`))
    
    df_gt_viz3 %>% 
      select(-c(merge_status_two)) %>%
      summary_tbl_ou_starter(viz_type = "03_pepfar_share_hp_phc") %>% 
      cols_label(phc_share = "(share)",
                 health_post_share = "(share)",
                 `Primary Health Center` = "PEPFAR Supported Primary Health Centers",
                 `Health Post` = "PEPFAR Supported Health Posts",
                 total_hp = "Total Health Posts" ,
                 total_phc = "Total Primary Health Centers") %>%
      tab_header(
        title = glue("SHARE OF PRIMARY HEALTH CENTERS AND HEALTH POSTS SUPPORTED BY PEPFAR")
      ) %>%
      gt_color_rows(columns = c(4,7), na.color = "white",
                    palette = c("#f7f7f7", golden_sand)) %>% 
      gtsave_extra(filename = glue("Images/TABLE_3_PEPFAR_PHC_HP.png"))  
    
      
    
  
