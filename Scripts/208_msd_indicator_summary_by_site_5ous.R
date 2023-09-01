# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  remake of TX_NEW table for 4 ous
# REF ID:   e84b7b90 
# LICENSE:  MIT
# DATE:     2023-08-31
# UPDATED: 
# derived from script 108

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

ref_id <- "e84b7b90"

# SI specific paths/functions   
load_secrets()
source("Scripts/00_helper_functions.R")

ou_list <- c("Kenya", "Malawi", "Nigeria", "Cote d'Ivoire")

# Sites pulled via API by AC
ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
#msd_path <- 

# API CODE to get site counts      
#https://github.com/USAID-OHA-SI/groundhog_day/blob/5a0fffd5b0848048fe805ba4a3a96e8281170f09/Scripts/FY21Q4_pepfar-site-count.R#L253

# Use google drive to download file and then pass temp path through to vroom to open
temp <- tempfile(fileext = ".zip")
dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)

# REF ID for plots
ref_id <- "cce714cf"

get_metadata()

# LOAD DATA ============================================================================  

#attribute data from 103_download_daa_deou_facilities
df_daa_sa <- readRDS("Dataout/site_attrib_df_5_ous") %>% 
  filter(!is.na(`Facility Type`))


df_daa <- readRDS("Dataout/daa_fac_df_5_ous") %>% 
  filter(regionorcountry_name != "Ghana")

#df_hrh <- readRDS("Dataout/df_hrh_fac_phc") 

df_msd <- readRDS("Dataout/msd_site_dfs_5_ous") %>% 
  filter(fiscal_year == 2023,
         operatingunit != "West Africa Region",
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
  tidylog::left_join(., df_daa_sa, by = c("orgunit_internal_id" = "DATIM UID")) %>% 
  mutate(merge_daa_sa = ifelse(is.na(`Facility Type`), "DAA only", "DAA & SA")) %>% 
  filter(!is.na(`Facility Type`)) %>%
  collapse_fac_type(., unique_var = `Facility Type`) %>% 
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
make_msd_tbl(df_msd_phc, ou = "Malawi")

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
make_mer_sh_tbl(ou_sh_tbl_full, "Malawi") 

map(ou_list, ~make_mer_sh_tbl(ou_sh_tbl_full, .x) %>% 
      gtsave(filename = glue("Images/{.x}_msd_overall_summary.png")))


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
    mutate(cntrygap = "Focus Primary Impact OUs")
}

# Function to create a wide table of faciliity types, passing in an indicator and
# base dataframe formed above


format_spread_sh_tbl <- function(df, indic = "HTS_TST_POS") {
  
  # Create the table for a specific indicator, add totals
  df <-  spread_sh_tbl(df, indic_name = indic) %>% 
    rowwise() %>% 
    mutate_if(is.double, ~tidyr::replace_na(., 0)) %>% 
    mutate(Total = sum(c_across(c(2, 4, 6, 8, 10, 12)), na.rm = T))
  
  # Set the names, so we can use a list in cols_label below
  new_names <-  names(df) %>% gsub(".*_", "", .) %>% set_names(., names(df))
  
  
  df %>% 
    # select(-c(cntrygrp)) %>% 
    #  cols_hide(columns = c(cntrygap)) %>% 
    gt(rowname_col = "operatingunit", groupname_col = "cntrygap") %>% 
    fmt_number(columns = seq(2, 14, by = 2), decimals = 0) %>% 
    fmt_number(columns = Total, decimals = 0) %>% 
    fmt_percent(columns = seq(3, 13, by = 2), decimals = 0) %>% 
    sub_missing(missing_text = ".") %>%
    cols_label(.list = new_names)  %>% 
    cols_label(`NA` = "Missing Info") %>% 
    #cols_hide(columns = c("cntrygap")) %>% 
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
    ) 
}

format_spread_sh_tbl(ou_sh_tbl_full, indic = "TX_NEW") %>% 
  gtsave(filename = glue("Images/TX_NEW_msd_overall_summary_4ous.png"))


