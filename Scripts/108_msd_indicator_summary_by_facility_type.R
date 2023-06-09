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
    #msd_path <- 
    
  # API CODE to get site counts      
  #https://github.com/USAID-OHA-SI/groundhog_day/blob/5a0fffd5b0848048fe805ba4a3a96e8281170f09/Scripts/FY21Q4_pepfar-site-count.R#L253
  
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
    
    df_hrh <- readRDS("Dataout/df_hrh_fac_phc") 
    
    df_msd <- readRDS("Dataout/msd_site_dfs") %>% 
      filter(fiscal_year == 2023,
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age/Sex/Result", 
                                             "Age/Sex", "Age Aggregated/Sex/HIVStatus",
                                             "Age/Sex/Indication/HIVStatus")) %>%
      clean_indicator() %>% 
      mutate(age_bands = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Pediatrics",
        ageasentered %in% c("15-19", "20-24") ~ "AYPs",
        TRUE ~ "Adults"
      )) %>%
      summarise(indic_value = sum(cumulative, na.rm = T), .by = c(operatingunit, orgunituid, age_bands, sex, indicator, facility))
    
    df_msd_vl <- df_msd %>% 
      filter(str_detect(indicator, "TX_PVLS")) %>% 
      pivot_wider(names_from = indicator, values_from = indic_value)
    
    # API info on whether or not a site has TA / DSD
    df_api <- vroom::vroom(dl$local_path) %>% 
      select(operatingunit:indicatortype) %>% 
      filter(operatingunit %in% ou_list, sitetype == "Facility") %>% 
      mutate(value = 1) %>% 
      pivot_wider(names_from = indicatortype, values_from = value) %>% 
      mutate(pepfar_fp = "PEPFAR") 
      
    df_api %>% count(operatingunit, pepfar_fp) %>% arrange(desc(n)) %>% janitor::adorn_totals()
    
    # What support types are covered under "No support type"
    vroom::vroom(dl$local_path) %>% filter(indicatortype == "No Support Type") %>% 
      pivot_longer(cols = where(is.logical)) %>% 
      filter(!is.na(value), sitetype == "Facility") %>% 
      count(name) %>% 
      pull(name)
    
    # What indicators are covered under TA or DSD?
    vroom::vroom(dl$local_path)  %>% 
      pivot_longer(cols = where(is.logical)) %>% 
      filter(!is.na(value), sitetype == "Facility") %>% 
      count(name) %>% 
      pull(name) %>% 
      writeLines
    
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
        mutate(VLS_Female = TX_PVLS_Female / TX_PVLS_D_Female,
               VLS_Male = TX_PVLS_Male / TX_PVLS_D_Male) %>% 
        select(-contains("PVLS")) %>% 
        gt(groupname_col = "age_bands") %>% 
        summary_rows(groups = c("Pediatrics", "AYPs", "Adults"),
                     columns = 2:8,
                     fns = list(
                       subtotal = ~sum(., na.rm = T)),
                     formatter = fmt_number,
                     decimals = 0
        ) %>% 
        grand_summary_rows(columns = 2:10,
                           fns = list(
                             overall = ~sum(., na.rm = T)), 
                           formatter = fmt_number,
                           decimals = 0
        ) %>% 
        sub_missing(missing_text = ".") %>% 
        fmt_number(columns = 3:10,
                   decimals = 0) %>% 
        fmt_percent(columns = 11:12, 
                    decimals = 0) %>% 
        cols_label(fac_type = "", 
                   HTS_TST_POS_Female = "Female",
                   HTS_TST_POS_Male = "Male",
                   TX_CURR_Female = "Female",
                   TX_CURR_Male = "Male",
                   TX_NEW_Female = "Female", 
                   TX_NEW_Male = "Male",
                   PrEP_NEW_Female = "Female",
                   PrEP_NEW_Male = "Male",
                   VLS_Female = "Female",
                   VLS_Male = "Male") %>% 
        tab_spanner(columns = 3:4,
                    label = "HTS_TST_POS") %>%
        tab_spanner(columns = 5:6,
                    label = "PREP_NEW") %>% 
        tab_spanner(columns = 7:8,
                    label = "TX_CURR") %>% 
        tab_spanner(columns = 9:10,
                    label = "TX_NEW") %>% 
        tab_spanner(columns = 11:12,
                    label = "VLS") %>% 
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
  make_msd_tbl(df_msd_phc, ou = "Zambia")
    
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
    left_join(., ou_mer_site_count) %>% 
      mutate(VLS = TX_PVLS/TX_PVLS_D, .after = TX_NEW_share) %>% 
      mutate(VLS_share = TX_PVLS_share, .after = VLS) %>% 
      select(-contains("PVLS")) 


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
      fmt_percent(columns = c(3, 5, 7, 9, 10, 11), decimals = 0) %>% 
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
        columns = c(2, 4, 6, 8, 12),
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
      ) %>% 
      cols_label(PrEP_NEW_share = "",
                 VLS_share = "")
  }
  
  # Pick an OU and test, then batch
  make_mer_sh_tbl(ou_sh_tbl_full, "Zambia") 
  
  map(ou_list, ~make_mer_sh_tbl(ou_sh_tbl_full, .x) %>% 
        gtsave(filename = glue("Images/{.x}_msd_overall_summary.png")))

  
  df_phc %>% 
    filter(regionorcountry_name == "Uganda") %>% 
    count(value)
  
  

# PIVOT INDICATORS WIDER --------------------------------------------------

  
  # Function to spread the data out wide, based on a single indicator
  # Used as a first step to generate the summary MER tables by OU
  spread_sh_tbl <- function(df, indic_name = "HTS_TST_POS"){
   
     df %>% 
      select(operatingunit, fac_type, contains(indic_name)) %>% 
      ungroup() %>% 
      pivot_longer(cols = where(is.double),
                   names_to = "indicator",
                   values_to = "value") %>% 
      arrange(fac_type) %>% 
      pivot_wider(names_from = c(fac_type, indicator), 
                  values_from = value) %>% 
      rename_with(~str_remove_all(., str_c("_", indic_name, sep = ""))) %>% 
      arrange(desc(`Primary Health Center`)) %>% 
      mutate(
        cntrygrp = case_when(
          str_detect(operatingunit, "Moz|Malawi|Zimb|Zam|South Africa|Uga") ~ "Large TX OUs",
          TRUE ~ "Small TX OUs"
        ),
        cntrygrp = fct_relevel(cntrygrp, c("Large TX OUs", "Small TX OUs"))
      )
    }
  
  # Function to create a wide table of faciliity types, passing in an indicator and
  # base dataframe formed above
  format_spread_sh_tbl <- function(df, indic = "HTS_TST_POS") {
    
    # Create the table for a specific indicator, add totals
    df <-  spread_sh_tbl(df, indic_name = indic) %>% 
      rowwise() %>% 
      mutate(Total = sum(c_across(c(2, 4, 6, 8, 10, 12, 14)), na.rm = T))
    
    # Set the names, so we can use a list in cols_label below
    new_names <-  names(df) %>% gsub(".*_", "", .) %>% set_names(., names(df))
    
    df %>% 
      gt(rowname_col = "operatingunit", groupname_col = "cntrygrp") %>% 
      fmt_number(columns = seq(2, 14, by = 2), decimals = 0) %>% 
      fmt_number(columns = Total, decimals = 0) %>% 
      fmt_percent(columns = seq(3, 15, by = 2), decimals = 0) %>% 
      sub_missing(missing_text = ".") %>%
      cols_label(.list = new_names)  %>% 
      gt_theme_phc() %>% 
      tab_header(
        title = glue("{indic}: {metadata$curr_pd} MER SUMMARY BY FACILITY TYPE")
      ) %>% 
      tab_source_note(
        source_note = gt::md(glue("Source: {metadata$source} & DATIM DAA Dataset | Ref id: {ref_id}"))) %>% 
    #   tab_options(
    #     source_notes.font.size = px(10)) %>% 
    # gt_highlight_cols(
    #   columns = c(2, 4, last_col()),
    #   fill = grey10k,
    #   font_weight = 600,
    #   alpha = 0.45
    # ) %>% 
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
      tab_style(
        style = list(
          cell_fill(color = old_rose, alpha = 0.15), 
          cell_text(weight = 700)
        ),
        locations = cells_body(
          rows = `Primary Health Center_share` > 0.5,
          columns = c(1, 2, 3, 4, 5)
        )
      ) %>% 
      drkn_clmn_hdr()
  }
  
  
  # Batch indicator tables
  indic_list <- c("PrEP_NEW", "HTS_TST_POS", "TX_CURR", "TX_NEW")
  map(indic_list, ~format_spread_sh_tbl(ou_sh_tbl_full, indic = .x) %>% 
        gtsave(filename = glue("Images/{.x}_msd_overall_summary.png")))

  # VLS Summary table
  format_spread_sh_tbl_vls <- function(df, indic = "VLS") {
    
    # Create the table for a specific indicator, add totals
    df <-  spread_sh_tbl(df, indic_name = indic) %>% 
      arrange(cntrygrp)
    
    # Set the names, so we can use a list in cols_label below
    new_names <-  names(df) %>% gsub(".*_", "", .) %>% set_names(., names(df))
    
    df %>% 
      gt(rowname_col = "operatingunit", groupname_col = "cntrygrp") %>% 
      # fmt_number(columns = seq(2, 14, by = 2), decimals = 0) %>% 
      # fmt_number(columns = Total, decimals = 0) %>% 
      fmt_percent(columns = seq(1, 15, by = 1), decimals = 0) %>% 
      sub_missing(missing_text = ".") %>%
      cols_label(.list = new_names)  %>% 
      gt_theme_phc() %>% 
      tab_header(
        title = glue("{indic}: {metadata$curr_pd} MER SUMMARY BY FACILITY TYPE")
      ) %>% 
      tab_source_note(
        source_note = gt::md(glue("Source: {metadata$source} & DATIM DAA Dataset | Ref id: {ref_id}"))) %>% 
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
      tab_style(
        style = list(
          cell_fill(color = old_rose, alpha = 0.15), 
          cell_text(weight = 700)
        ),
        locations = cells_body(
          rows = `Primary Health Center_share` >= 0.5,
          columns = c(1, 2, 3, 4, 5)
        )
      ) 
  }
  
  format_spread_sh_tbl_vls(ou_sh_tbl_full, indic = "VLS") %>% 
    gtsave(filename = glue("Images/VLS_msd_overall_summary.png"))
  

# Summary Table of Site Attributes by OU ----------------------------------

  df_phc %>%
    count(regionorcountry_name, value) %>%
    mutate(value = ifelse(is.na(value), "Missing info", value)) %>%
    spread(value, n) %>%
    relocate(c(3, 4, 8), .after = 1) %>%
    relocate(4, .after = 1) %>%
    relocate(6, .after = last_col()) %>%
    arrange(desc(`Primary Health Center`)) %>%
    rowwise() %>% 
    mutate(Total = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
    mutate(`Share` = (sum(c_across(c(2, 3)), na.rm = T) / Total), .after = `Health Post`) %>% 
    ungroup() %>% 
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
    fmt_number(columns = where(is.integer), 
               decimals = 0) %>% 
    fmt_percent(columns = where(is.double), 
                decimals = 0) %>%
    tab_spanner(
      columns = c(2, 3, 4),
      label = "focus facilities", gather = T
    ) %>%
    grand_summary_rows(
      columns = where(is.integer),
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
    ) %>% 
    gt_highlight_cols(
      columns = c(2:4, Total),
      fill = grey10k,
      font_weight = 600,
      alpha = 0.45
    ) 
  
  %>% 
    gtsave(filename = glue("Images/daa_site_overall_summary.png"))
  

# MAKE WAFFLE SUMMARY -- MMMM Wafles.... ----------------------------------
  
  #munge HRH
  df_hrh %>% 
    filter(operatingunit %in% ou_list) %>%
    distinct
    
    
   #janitor::get_dupes() %>% 
    #nrow() %>% 
    count(any_hrh, merge_daa_sa)
    
  

  library(waffle)
  library(extrafont)
  make_waffle <- function(n, clr = grey20k){
    c(n) %>% 
      as.table() %>% 
      waffle::waffle(color = clr, flip = T,
                     reverse = T, size = 0.5) +
      theme(legend.position = "none")
  }
  
  # Get BANS
  daa_ban <- df_daa %>% nrow() 
  api_ban <- df_api %>% nrow()
  daa_sa_ban <- df_daa_sa %>% nrow()
  msd_ban <- df_msd %>% distinct(orgunituid) %>% nrow()
  hrh_ban <- df_hrh %>% filter(operatingunit %in% ou_list) %>% count(any_hrh) %>% pull(n)
  hrh_sa_ban <- df_hrh %>% filter(operatingunit %in% ou_list) %>% count(any_hrh,merge_daa_sa) %>% 
                filter(merge_daa_sa == "DAA & SA") %>% pull(n)
  
  
  # GET SHARES
  api_prop <- api_ban / daa_ban
  daa_sa_prop <- daa_sa_ban / daa_ban
  msd_prop <- msd_ban / daa_ban
  hrh_prop <- hrh_ban / daa_ban
  hrh_sa_prop <- hrh_sa_ban / daa_ban
  
  
  gen_tile_fill <- function(prop){
    x <- round(prop, 2) * 100
    y <- 100 - (round(prop, 2) * 100)
    return(c(x, y))
  }
  
gen_tile_fill(api_prop)
  x_pos = 3
  y_pos = 9.5
  fontfam = "Soure Sans Pro"

  daa <- make_waffle(100) + labs(title = "DAA Sites") + 
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(daa_ban), size = 26/.pt, family = fontfam, fontface = 2)
  
  api <- make_waffle(gen_tile_fill(api_prop), clr = c(scooter_med, grey20k)) + labs(title = "DATIM API Sites") +
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(api_ban), size = 26/.pt, family = fontfam, fontface = 2)
  
  daa_sa <- make_waffle(gen_tile_fill(daa_sa_prop), clr = c("#D67288", grey20k)) + labs(title = "DAA Site Attributes") +
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(daa_sa_ban), size = 26/.pt, family = fontfam, fontface = 2 )
  
  msd <- make_waffle(gen_tile_fill(msd_prop), clr = c("#D6CE47", grey20k)) + labs(title = "Site-Level MSD") +
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(msd_ban), size = 26/.pt, family = fontfam, fontface = 2 )
  
  hrh <- make_waffle(gen_tile_fill(hrh_prop), clr = c("#5B82BD", grey20k)) + labs(title = "HRH Inventory Sites") +
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(hrh_ban), size = 26/.pt, family = fontfam, fontface = 2 )
  
  hrh_sa <- make_waffle(gen_tile_fill(hrh_sa_prop), clr = c("#877EC9", grey20k)) + labs(title = "HRH Inventory Sites with Attribute Information") +
    annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(hrh_sa_ban), size = 26/.pt, family = fontfam, fontface = 2 )
  
  daa + daa_sa + api + msd + plot_layout(nrow = 1) 
    si_save("Graphics/PHC_data_source_summary.svg")
  
  df_phc %>% count(merge_daa_api)
    
  daa + daa_sa + hrh + hrh_sa + plot_layout(nrow = 1) 
  si_save("Graphics/hrh_data_source_summary.svg")
  