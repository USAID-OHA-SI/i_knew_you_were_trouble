# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Remake summary attribute table (sorted and highlighted)
# AUTHOR: Tim Essam | SI
# REF ID:   0062fc80
# LICENSE: MIT
# DATE: 2023-04-13
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
    
    
  # SI specific paths/functions  
    load_secrets()
    source("Scripts/00_helper_functions.R")
  
  # Sites pulled via API by AC
    ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
    msd_path <- 
  
  # Use google drive to download file and then pass temp path through to vroom to open
    temp <- tempfile(fileext = ".zip")
    dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
      
  # REF ID for plots
    ref_id <- "cce714cf"
  
# LOAD DATA ============================================================================  

    df_daa_sa <- readRDS("Dataout/site_attrib_df") %>% 
      filter(dataelement == "SA_FACILITY_TYPE") 
    
    df_daa <- readRDS("Dataout/daa_fac_df") 
    
    df_msd <- readRDS("Dataout/msd_site_dfs") %>% 
      filter(fiscal_year == 2023,
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age/Sex/Result")) %>%
      mutate(age_bands = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Pediatrics",
        ageasentered %in% c("15-19", "20-24") ~ "AYPs",
        TRUE ~ "Adults"
      )) %>%
      summarise(indic_value = sum(cumulative, na.rm = T), .by = c(operatingunit, orgunituid, age_bands, sex, indicator, facility))
    
    # API info on whether or not a site has TA / DSD
    df_api <- vroom::vroom(dl$local_path) %>% 
      select(operatingunit:indicatortype) %>% 
      filter(operatingunit %in% ou_list, sitetype == "Facility") %>% 
      mutate(value = 1) %>% 
      pivot_wider(names_from = indicatortype, values_from = value) %>% 
      mutate(pepfar_fp = case_when(
        is.na(DSD) & is.na(TA) ~ "Non-PEPFAR",
        TRUE ~ "PEPFAR"
      )) %>% 
      arrange(pepfar_fp)
      
    df_api %>% count(operatingunit, pepfar_fp) %>% spread(pepfar_fp, n) %>% 
      mutate(total = `Non-PEPFAR` + PEPFAR) %>% 
      arrange(desc(total))

# MUNGE ============================================================================
  
   df_phc <- df_daa %>% 
      tidylog::left_join(., df_api, by = c("orgunit_internal_id" = "orgunituid")) %>% 
      mutate(merge_daa_api = ifelse(is.na(pepfar_fp), "DAA only", "DAA & API")) %>% 
      tidylog::left_join(., df_daa_sa, by = c("orgunit_internal_id" = "orgunit"))%>% 
      mutate(merge_daa_sa = ifelse(is.na(dataelement), "DAA only", "DAA & SA")) %>% 
      collapse_fac_type() %>% 
      relevel_fac_type() %>% 
      mutate(pepfar_fp_adj = ifelse(is.na(pepfar_fp), "Non-PEPFAR", pepfar_fp),
             pepfar_fp_adj = fct_relevel(pepfar_fp_adj, c("PEPFAR", "Non-PEPFAR")))
   
    
  
# VIZ ============================================================================

  # Pick a country and document process
    
    df_msd %>% filter(operatingunit == "Uganda") %>% 
      left_join(., df_phc, by = c("orgunituid" = "orgunit_internal_id")) %>% 
      mutate(fac_type = case_when(
               str_detect(facility, "Data reported") ~ "Above site", 
               TRUE ~ fac_type)
      ) %>% 
      relevel_fac_type() %>% 
      mutate(age_bands = fct_relevel(age_bands, c("Pediatrics", "AYPs", "Adults"))) %>% 
      group_by(fac_type, indicator, age_bands, sex) %>% 
      summarise(indic_value = sum(indic_value, na.rm = T)) %>% 
      mutate(indicator = str_c(indicator, sex, sep = "_")) %>% 
      select(-sex) %>% 
      spread(indicator, indic_value) %>% 
      arrange(age_bands, fac_type) %>% 
      gt(groupname_col = "age_bands") %>% 
      summary_rows(groups = c("Pediatrics", "AYPs", "Adults"),
                   columns = 2:8,
                   fns = list(
                     subtotal = ~sum(., na.rm = T)),
                   formatter = fmt_number,
                   decimals = 0
      ) %>% 
      grand_summary_rows(columns = is.numeric,
                              fns = list(
                                Overall = ~sum(., na.rm = T)), 
                              formatter = fmt_number,
                              decimals = 0
      ) %>% 
      sub_missing(missing_text = ".") %>% 
      gt_theme_nytimes() %>% 
      fmt_number(columns = is.numeric,
                 decimals = 0) %>% 
      cols_label(fac_type = "", 
                 HTS_TST_POS_Female = "Female",
                 HTS_TST_POS_Male = "Male",
                 TX_CURR_Female = "Female",
                 TX_CURR_Male = "Male",
                 TX_NEW_Female = "Female", 
                 TX_NEW_Male = "Male") %>% 
      tab_spanner(columns = 3:4,
                  label = "HTS_TST_POS") %>% 
      tab_spanner(columns = 5:6,
                  label = "TX_CURR") %>% 
      tab_spanner(columns = 7:8,
                  label = "TX_NEW") %>% 
      tab_options(
        table.font.size = px(12),
        column_labels.font.size = px(14),
        row_group.font.weight = "bold",
        row_group.font.size = px(14),
        data_row.padding = px(1), 
        summary_row.padding = px(1.5),
        grand_summary_row.padding = px(1.5),
      ) %>% 
      row_highlight(rowvar = fac_type) %>% 
      cols_align(align = "left", columns = 1) 
      

# SPINDOWN ============================================================================

