# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  summary table across OUs
# REF ID:   5ec12c97 
# LICENSE:  MIT
# DATE:     2023-08-17
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

ref_id <- "5ec12c97"

# IMPORT ------------------------------------------------------------------

#

ou_list <- c("Kenya", "Malawi", "Nigeria", "Cote d'Ivoire")

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
df_daa <- readRDS("Dataout/daa_fac_df_5_ous") 

#attribute data from 103_download_daa_deou_facilities
df_daa_sa <- readRDS("Dataout/site_attrib_df_5_ous") %>% 
  filter(!is.na(`Facility Type`))

# PRIMARY IMPACT XWALK ----------------------------------------------------

#Read in MSD
df_msd <- readRDS("Dataout/msd_site_dfs_5_ous")

#pull out hierachies
snu_map <- df_msd %>% 
  filter(operatingunit != "West Africa Region") %>% 
  count(operatingunit, operatingunituid, snu1, snu1uid, psnu, psnuuid, sitename, facilityuid)

#google id for Primary impact district xwalk
g_id <- "10KLNSuu6nZtV0p2FOS7dY8by0_Fa9gvyLUH1zKqBAvc"
pi_districts <- read_sheet(g_id)


#Malawi - districts at the psnu level
mwi_map <- snu_map %>% 
  select(-n) %>% 
  filter(operatingunit == "Malawi") %>% 
  left_join(pi_districts %>%
              filter(level3name == "Malawi") %>% select(uidlevel3, level3name, uidlevel6, level6name),
            by = c('operatingunit' = 'level3name', 'operatingunituid' = 'uidlevel3', 'psnuuid' = 'uidlevel6')) %>% 
  filter(!is.na(level6name)) %>% 
  select(-c(snu1, snu1uid)) %>% 
  rename(district = psnu,
         district_uid = psnuuid) %>% 
  select(-c(level6name))

#Kenya, Nigeria, CDI - snu1 level
snu_map_join <- snu_map %>% 
  select(-n) %>% 
  filter(operatingunit != "Malawi") %>% 
  left_join(pi_districts %>%
              filter(level3name != "Malawi") %>% select(uidlevel3, level3name, uidlevel4, level4name),
            by = c('operatingunit' = 'level3name', 'operatingunituid' = 'uidlevel3', 'snu1uid' = 'uidlevel4')) %>% 
  filter(!is.na(level4name)) %>% 
  select(-c(psnu, psnuuid)) %>% 
  rename(district = snu1,
         district_uid = snu1uid) %>% 
  select(-c(level4name)) %>% 
  rbind(mwi_map)

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
  tidylog::left_join(df_daa_sa, by = c("orgunit_internal_id" = "DATIM UID")) %>% 
  mutate(merge_attributes = ifelse(is.na(`Facility Type`), "DAA only", "DAA & Attrb")) %>% 
  tidylog::left_join(api_tagged, by = c("orgunit_internal_id" = "orgunituid")) %>% 
  mutate(merge_api = ifelse(is.na(sitetype), "DAA only", "DAA & api")) %>% 
  mutate(merge_status_two = ifelse(is.na(sitename), "non-PEPFAR", "PEPFAR")) %>% 
  rename(orgunituid= orgunit_parent_internal_id) %>% 
  tidylog::left_join(snu_map_join %>% select(-operatingunit, sitename), by = c("orgunit_internal_id" = "facilityuid"))

# SUMMARY TABLES BY OU -----------------------------------------------------------------------------

ou_totals_daa <- get_ou_site_totals(df_gt_all)


create_joined_tbl <- function(ou) {
  
  #OU level summary
  df_gt_ou_tbl <- df_gt_all %>% 
    filter(regionorcountry_name == ou) %>% 
    filter(!is.na(`Facility Type`)) %>%
    #  mutate(fac_type = collapse_2())
    collapse_fac_type(., unique_var = `Facility Type`) %>%
    distinct(regionorcountry_name, orgunit_internal_id, merge_status_two, fac_type) %>%
    count(regionorcountry_name, merge_status_two, fac_type, sort = T) %>% 
    pivot_wider(names_from = "fac_type", values_from  = "n")
  
  col_names <- names(df_gt_ou_tbl)
  
  if ("Health Post" %ni% col_names) {
    df_gt_ou_tbl <- df_gt_ou_tbl %>% 
      mutate("Health Post" = NA)
  }
  
  df_gt_ou_tbl <- df_gt_ou_tbl %>%
    mutate(`Mobile Health Clinic` = NA) %>% 
    filter(merge_status_two == "PEPFAR") %>% 
    left_join(ou_totals_daa) %>% 
    mutate(phc_share = `Primary Health Center`/ total_pepfar_sites,
           health_post_share = `Health Post`/ total_pepfar_sites,
           other_share = `Other Facility` / total_pepfar_sites,
           mobile_share = `Mobile Health Clinic` / total_pepfar_sites,
           hospital_share = Hospital / total_pepfar_sites) %>% 
    rename(Geography = regionorcountry_name) %>% 
    #mutate(District = NA) %>% 
    select(Geography, `Primary Health Center`, phc_share, `Health Post`, health_post_share, 
           `Mobile Health Clinic`, mobile_share, Hospital, hospital_share, `Other Facility`, 
           other_share, total_pepfar_sites) %>% 
    arrange(desc(phc_share))
  
  #get district totals
  df_totals_district <- df_gt_all %>% 
    filter(regionorcountry_name == ou) %>% 
    mutate(indicator_type = case_when(DSD ==1 & TA ==1 ~ "DSD & TA",
                                      DSD == 1 & is.na(TA) ~ "DSD",
                                      TA == 1 & is.na(DSD) ~ "TA",
                                      is.na(DSD) & is.na(TA) & merge_status_two == "PEPFAR" ~ "HSS Sites",
                                      TRUE ~ "Not PEPFAR Supported")) %>% 
    distinct(regionorcountry_name, orgunit_internal_id, district, merge_status_two, indicator_type) %>%
    count(regionorcountry_name, district, indicator_type, sort = T) %>% 
    group_by(district) %>%
    mutate(total_sites = sum(n)) %>% 
    ungroup() %>% 
    filter(indicator_type != "Not PEPFAR Supported") %>% 
    group_by(district) %>%
    mutate(total_pepfar_sites = sum(n)) %>%
    ungroup() %>% 
    distinct(regionorcountry_name, district, total_sites, total_pepfar_sites)
  
  #District table
  df_gt_district_tbl <- df_gt_all %>% 
    filter(regionorcountry_name == ou) %>% 
    filter(!is.na(`Facility Type`)) %>%
    #  mutate(fac_type = collapse_2())
    collapse_fac_type(., unique_var = `Facility Type`) %>%
    distinct(regionorcountry_name, orgunit_internal_id, district, merge_status_two, fac_type) %>%
    count(regionorcountry_name, district, merge_status_two, fac_type, sort = T) %>% 
    pivot_wider(names_from = "fac_type", values_from  = "n") %>%
    filter(!is.na(district))
  
  col_names_district <- names(df_gt_district_tbl)
  
  if ("Health Post" %ni% col_names_district) {
    df_gt_district_tbl <- df_gt_district_tbl %>% 
      mutate("Health Post" = NA)
  }  
  
  df_gt_district_tbl <- df_gt_district_tbl %>% 
    mutate(`Mobile Health Clinic` = NA) %>% 
    filter(merge_status_two == "PEPFAR") %>% 
    left_join(df_totals_district) %>% 
    mutate(phc_share = `Primary Health Center`/ total_pepfar_sites,
           health_post_share = `Health Post`/ total_pepfar_sites,
           other_share = `Other Facility` / total_pepfar_sites,
           mobile_share = `Mobile Health Clinic` / total_pepfar_sites,
           hospital_share = Hospital / total_pepfar_sites) %>% 
    rename(Geography = district) %>% 
    select(Geography, `Primary Health Center`, phc_share, `Health Post`, health_post_share, 
           `Mobile Health Clinic`, mobile_share, Hospital, hospital_share, `Other Facility`, 
           other_share, total_pepfar_sites) %>% 
    arrange(desc(phc_share))
  
  joined_tbl <- df_gt_ou_tbl %>% 
    rbind(df_gt_district_tbl)
  
  return(joined_tbl)
}


ou_tbl_starter <- function(df) {

    pct_cols <- c(3,5,7,9,11)
    num_cols <- c(2,4,6,8,10)
    col_breaks <- c(2, 4,6,8,10,12)
    bold_col <- c(12)
  
  df %>% 
    gt() %>% 
    sub_missing(missing_text = ".",
    ) %>% 
    fmt_percent(columns = pct_cols,
                decimals = 0) %>%
    fmt_number(columns = num_cols,
               decimal = 0) %>%
    # cols_label(phc_share = "(share)",
    #            health_post_share = "(share)", 
    #            `Primary Health Center` = "PEPFAR Supported Primary Health Centers",
    #            `Health Post` = "PEPFAR Supported Health Posts",
    #            total_hp = "Total Health Posts" ,
    #            total_phc = "Total Primary Health Centers") %>% 
    cols_align(align = "left", columns = 1) %>% 
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
          columns = col_breaks
        )
      )
    ) %>% 
    # grand_summary_rows(
    #   columns = where(is.integer),
    #   fns = list(
    #     Overall = ~ sum(., na.rm = T)
    #   ),
    #   formatter = fmt_number,
    #   decimals = 0
    # ) %>% 
    gt_theme_nytimes() %>% 
    # tab_header(
    #   title = glue("SHARE OF PRIMARY HEALTH CENTERS AND HEALTH POSTS SUPPORTED BY PEPFAR")
    # ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM DAA Site Attribute Data | Ref id: {ref_id}"))) %>% 
    tab_options(
      source_notes.font.size = px(10),
      row_group.font.weight = "bold",
      data_row.padding = px(1),
      column_labels.font.size = px(15)) %>% 
    # Highlighting max value within each column
    # gt_color_rows(columns = c(4,7), na.color = "white", 
    #               palette = c("#f7f7f7", golden_sand)) %>% 
    tab_style(
      style = list(
        cell_text(weight = 600)
      ),
      locations = cells_body(
        columns = bold_col
      )
    )
  
}

primary_impact_viz_tbl <- function(df, ou) {
  
  if (ou == "Malawi") {
    phc_low_bnd <- 0.6
    phc_up_bnd <- 0.75
    hp_low_bnd <- 0
    hp_up_bnd <- 0.75
  } else if (ou == "Nigeria") {
    phc_low_bnd <- 0.25
    phc_up_bnd <- 0.60
    hp_low_bnd <- 0
    hp_up_bnd <- 0.60
  } else if (ou == "Cote d'Ivoire") {
    phc_low_bnd <- 0.65
    phc_up_bnd <- 0.73
    hp_low_bnd <- 0
    hp_up_bnd <- 0.75
  } else if (ou == "Kenya") {
    phc_low_bnd <- 0.1
    phc_up_bnd <- 0.4
    hp_low_bnd <- 0
    hp_up_bnd <- 0.75
  }
  
  
  df %>% 
    ou_tbl_starter() %>% 
    cols_label(phc_share = "(share)",
               health_post_share ="(share)", 
               mobile_share = "(share)",
               hospital_share = "(share)",
               other_share = "(share)",
               total_pepfar_sites = "Total PEPFAR-supported facilities") %>% 
    tab_row_group(
      label = "Primary Impact District",
      rows = Geography %ni% ou_list
    ) %>%
    tab_row_group(
      label = "Country",
      rows = Geography %in% ou_list
    ) %>%
    tab_header(
      title = glue("{ou %>% toupper}: SHARE OF PEPFAR-SUPPORTED PHCS & HEALTH POSTS BY TOTAL PEPFAR-SUPPORTED FACILITIES WITHIN OU")
    ) %>% 
    gt_color_rows(columns = c(3), na.color = "white", 
                  palette = c("#f7f7f7", scooter_med),
                  domain = range(phc_low_bnd, phc_up_bnd),
                  pal_type = "continuous") %>% 
    gt_color_rows(columns = c(5), na.color = "white", 
                  palette = c("#f7f7f7", scooter_med),
                  domain = range(hp_low_bnd, hp_up_bnd),
                  pal_type = "continuous") %>% 
    # gt_color_rows(columns = c(5), na.color = "white", 
    #               palette = c("#f7f7f7", scooter_med),
    #               domain = c(5),
    #               pal_type = "continuous") %>% 
    drkn_clmn_hdr() 
}


make_primary_impact_viz <- function(ou) {
  create_joined_tbl(ou) %>% 
    primary_impact_viz_tbl(ou)
}

  #apply function
make_primary_impact_viz("Kenya")

create_joined_tbl("Nigeria") %>% 
  primary_impact_viz_tbl("Nigeria")

map(ou_list, ~make_primary_impact_viz(ou = .x) %>% 
      gtsave_extra(filename = glue("Images/{.x}_primary_impact_summary.png")))

# MAKE WAFFLE SUMMARY -- MMMM Wafles.... ----------------------------------

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
daa_ban <- df_daa %>% filter(regionorcountry_name != "Ghana") %>% nrow() 
api_ban <- api_tagged %>% nrow()
daa_sa_ban <- df_daa_sa %>% nrow()
msd_ban <- df_msd %>% distinct(orgunituid) %>% nrow()

# GET SHARES
api_prop <- api_ban / daa_ban
daa_sa_prop <- daa_sa_ban / daa_ban
msd_prop <- msd_ban / daa_ba


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

daa + daa_sa + api + msd + plot_layout(nrow = 1) 
si_save("Graphics/PHC_data_source_summary_4_ous.svg")




