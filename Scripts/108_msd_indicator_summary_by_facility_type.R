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
    library(googledrive)
    library(gtExtras)
    library(gt)
    
    
  # SI specific paths/functions  
    load_secrets()
    source("Scripts/00_helper_functions.R")
  
  # Sites pulled via API by AC
    ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
    msd_path <- 
  
  # Use google drive to download file and then pass temp path through to vroom to open
    temp <- tempfile(fileext = ".zip")
    dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
    
    get_metadata("Data/MER_Structured_Datasets_Site_IM_FY21-23_20230317_v2_1_Botswana.zip")
      
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
      tidylog::left_join(., df_daa_sa, by = c("orgunit_internal_id" = "orgunit")) %>% 
      mutate(merge_daa_sa = ifelse(is.na(dataelement), "DAA only", "DAA & SA")) %>% 
      collapse_fac_type() %>% 
      relevel_fac_type() 
    
    df_msd_phc <-  
      df_msd %>% 
      left_join(., df_phc, by = c("orgunituid" = "orgunit_internal_id")) %>% 
      mutate(fac_type = case_when(
        str_detect(facility, "Data reported") ~ "Above site", 
        TRUE ~ fac_type)
      ) %>% 
      relevel_fac_type() 
    
# VIZ ============================================================================
    
    make_msd_tbl <- function(df, ou){
      df %>% 
        filter(operatingunit == ou) %>% 
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
                                  overall = ~sum(., na.rm = T)), 
                                formatter = fmt_number,
                                decimals = 0
        ) %>% 
        sub_missing(missing_text = ".") %>% 
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
        gt_theme_phc() %>% 
        tab_options( # Adjusting padding between rows and font sizes
          table.font.size = px(12),
          column_labels.font.size = px(14),
          row_group.font.weight = "bold",
          row_group.font.size = px(14),
          data_row.padding = px(1), 
          summary_row.padding = px(1.5),
          grand_summary_row.padding = px(1.5),
          row_group.padding = px(2),
          heading.padding = px(1)
        ) %>% 
        row_highlight(rowvar = fac_type) %>% 
        cols_align(align = "left", columns = 1) %>% 
        tab_header(
          title = glue::glue("{str_to_upper(ou)} FY23 Q1 SUMMARY OF CORE INDICATORS BY FACILITY TYPE")
        ) %>% 
        tab_source_note(
          source_note = gt::md(glue::glue("DATIM Data Alignment Activity Attribute Data & DATIM API 2023"))
        ) %>% 
        tab_style(style = cell_text(
          font = google_font("Source Sans Pro"),
          weight = 600
        ), 
        locations = cells_grand_summary()
        ) %>% 
        tab_style(style = cell_text(
        font = google_font("Source Sans Pro"),
        weight = 400
          ),
        locations = cells_summary()
        )
    }

  # Test age/sex summary table 
  make_msd_tbl(df_msd_phc, ou = "South Africa")
    
  # Batch and save
  map(ou_list, ~make_msd_tbl(df_msd_phc, ou = .x) %>% 
        gtsave(filename = glue("Images/{.x}_msd_fac_summary.png")))
          

# SPINDOWN ============================================================================

  # Create facility / indicator summary tables with indicators rolled up to fac type
  # Need to create a base table + site counts to be joined on
  # Get site counts by facility type

  ou_mer_site_count <- 
    df_msd_phc %>% 
    distinct(orgunituid, fac_type, operatingunit) %>% 
    count(operatingunit, fac_type, name = "total_sites") %>% 
    mutate(total_sites = ifelse(fac_type == "Above site", NA_integer_, total_sites))
    
 # Full share table for looping over    
  ou_sh_tbl_full <-  
    df_msd_phc %>% 
      summarise(indic_value = sum(indic_value, na.rm = T), .by = c(operatingunit, fac_type, indicator)) %>% 
      spread(indicator, indic_value) %>% 
      group_by(operatingunit) %>% 
      mutate(across(c(where(is.double)), ~(.x / sum(.x, na.rm = T)), .names = "{.col}_share")) %>% 
      select(operatingunit, fac_type, order(colnames(.))) %>% 
    left_join(., ou_mer_site_count) 


  # Create a function to make summary rollup of MER indicators by OU
  # This will be batched over and should fit into the existing
  # `create_phc_gt()` function
  
    make_mer_sh_tbl <- function(df, ou){
    
      cntry <- str_to_upper(ou)
      
      df %>% 
        filter(operatingunit == ou) %>%  
        ungroup() %>% 
        select(-operatingunit) %>% 
        rename(facility_type = fac_type) %>% 
        create_phc_gt(cntry = ou) %>% 
        fmt_percent(columns = c(3, 5, 7), decimals = 0) %>% 
        fmt_number(columns = 8, decimals = 0) %>% 
        sub_missing(missing_text = ".") %>% 
        cols_label(total_sites = "Total Sites Reporting") %>% 
        gt_highlight_rows(
          rows = facility_type %in% c("Health Post", "Primary Health Center"),
          fill = grey10k,
          font_weight = 600,
          alpha = 0.45
        ) %>% 
        tab_options(
          column_labels.font.size = px(15)
        ) %>% 
        grand_summary_rows(
          columns = c(2, 4, 6, 8),
            fns = list(
              Overall = ~sum(., na.rm = T)), 
            formatter = fmt_number,
            decimals = 0
          ) %>% 
        tab_style(style = cell_text(
            font = google_font("Source Sans Pro"),
            weight = 600
          ), 
          locations = cells_grand_summary()
          )
    }

  # Pick an OU and test, then batch
  make_mer_sh_tbl(ou_sh_tbl_full, "Uganda") 
  
  map(ou_list, ~make_mer_sh_tbl(ou_sh_tbl_full, .x) %>% 
        gtsave(filename = glue("Images/{.x}_msd_overall_summary.png")))

  
  df_phc %>% 
    filter(regionorcountry_name == "Uganda") %>% 
    count(value)
  
  

# Summary Table of Site Attributes by OU ----------------------------------

  df_phc %>%
    count(regionorcountry_name, value) %>%
    mutate(value = ifelse(is.na(value), "Missing info", value)) %>%
    spread(value, n) %>%
    relocate(c(3, 4, 8), .after = 1) %>%
    relocate(4, .after = 1) %>%
    relocate(6, .after = last_col()) %>%
    arrange(desc(`Primary Health Center`)) %>%
    mutate(
      cntry_grp = case_when(
        str_detect(regionorcountry_name, "Moz|Malawi|Zimb|Zam|South Africa|Uga") ~ "Large TX OUs",
        TRUE ~ "Small TX OUs"
      ),
      cntry_grp = fct_relevel(cntry_grp, c("Large TX OUs", "Small TX OUs"))
    ) %>%
    arrange(cntry_grp) %>%
    gt(
      groupname_col = "cntry_grp",
      rowname_col = "regionorcountry_name"
    ) %>%
    fmt_number(columns = is.numeric, decimals = 0) %>%
    tab_spanner(
      columns = c(2, 3),
      label = "focus", gather = T
    ) %>%
    grand_summary_rows(
      columns = is.numeric,
      fns = list(
        Overall = ~ sum(., na.rm = T)
      ),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    sub_missing(missing_text = ".") %>%
    tab_header(
      title = glue::glue("SUMMARY OF OU SITE ATTRIBUTES")
    ) %>%
    gt_theme_phc() %>%
    tab_options(
      row_group.font.weight = "bold",
      row_group.font.size = px(14),
      data_row.padding = px(5),
      table.width = pct(75)
    ) %>%
    tab_source_note(
      source_note = gt::md(glue::glue("DATIM Data Alignment Activity Attribute Data 2023"))
    ) %>%
    tab_options(
      source_notes.font.size = px(10)
    ) %>%
    tab_style(
      style = cell_text(
        font = google_font("Source Sans Pro"),
        weight = 600
      ),
      locations = cells_grand_summary()
    )
    gtsave(filename = glue("Images/daa_site_overall_summary.png"))
    
  