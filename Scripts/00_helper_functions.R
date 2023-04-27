#Helper functions for the project

# Focus countries for the project
ou_list <- c("Botswana", "Cameroon", "Cote d'Ivoire", "Eswatini", 
             "Ethiopia", "Lesotho", "Malawi","Mozambique", 
             "Rwanda", "South Africa", "South Sudan", "Uganda",
             "Zambia", "Zimbabwe")

# Calculates the share for types of facilities
calc_share <- function(df, ...){
  df %>% 
    count(..., sort = T) %>% 
    mutate(Share = n / sum(n))
}


# Start a summary gt table for facility attributes
gt_starter <- function(df, ...){
  df %>% 
    gt() %>% 
    fmt_percent(columns = Share, 
                decimals = 0) %>%   
    grand_summary_rows(columns = ...,
                       fns = list(
                         Total = ~sum(.))
    ) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() 
}



# Pivots the attribute data wide so we can tabulate facility types easily
pivot_daa <- function(df){
  df %>% 
    filter(dataelement %in% c("SA_FACILITY_TYPE", "SA_OWN_TYPE")) %>%
    pivot_wider(names_from = dataelement, values_from = value) 
}


# munge the MSD for key indicators
munge_msd <- function(df){
  df %>% 
    filter(fiscal_year == metadata$curr_fy, standardizeddisaggregate == "Total Numerator",
           indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS")) %>% 
    group_by(fiscal_year, indicator, orgunituid, sitename) %>% 
    summarize(across(c(cumulative), \(x) sum(x, na.rm = T))) %>% 
    pivot_wider(names_from = indicator, values_from = cumulative)
}


# Function to join datim assets and show he join log
# Order of join is DEOU (MFL equivalent), DAA Attribute data, Site level MSD collapsed to indicators
#' Joins DATIM Assets
#'
#' @param df1 DATIM Exchange Organisation Units data frame (MFL equivalent)
#' @param df2 DATIM Attribute dataset
#' @param df3 DATIM MSD or API data
#' @param unique_var column used to flag merge outcome
#'
#' @return data frame of merged DATIM assets
#' @export
#'
join_datim_assets <- function(df1, df2, df3, unique_var = sitename){
  df <- 
    tidylog::left_join(df1, df2, by = c("orgunit_internal_id" = "orgunit")) %>% 
    rename(orgunituid = orgunit_internal_id) %>% 
    mutate(merge_status = ifelse(is.na(period), "deou_only", "merged")) %>% 
    tidylog::left_join(., df3) %>% 
    mutate(merge_status_two = ifelse(is.na({{unique_var}}), "non-PEPFAR", "PEPFAR"))
  return(df)
}                 


# OU gt summary data frame
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
create_coverage_df <- function(df){
  
  total_sites <- df %>% 
    filter(merge_status_two == "PEPFAR", !is.na(SA_FACILITY_TYPE)) %>%
    mutate(facility_type = case_when(
      is.na(SA_FACILITY_TYPE) ~ "Other",
      TRUE ~ SA_FACILITY_TYPE
    )) %>% 
    distinct(sitename) %>% 
    count() %>% pull()
  
  df %>% 
    filter(merge_status_two == "PEPFAR", !is.na(SA_FACILITY_TYPE)) %>%
    mutate(facility_type = case_when(
      is.na(SA_FACILITY_TYPE) ~ "Other",
      TRUE ~ SA_FACILITY_TYPE
    )) %>% 
    group_by(facility_type) %>% 
    summarize(across(c(HTS_TST_POS, TX_NEW, TX_CURR), \(x) sum(x, na.rm = T))) %>% 
    ungroup() %>% 
    mutate(across(c(HTS_TST_POS, TX_NEW, TX_CURR), \(x) percent(x / sum(x, na.rm = T), 1.0), .names = "{.col}_share"), 
           facility_type = fct_reorder(facility_type, TX_CURR, .desc = T)) %>% 
    arrange(facility_type) %>% 
    mutate(total_sites = total_sites)
}


# Extract OU name from joined data
# Store as object named cntry
get_countryname <- function(df){
  df %>% 
    distinct(regionorcountry_name) %>% 
    pull() %>% 
    str_to_upper()
}


create_phc_gt <- function(df, cntry = ""){
  
  cntry <- str_to_upper(cntry)
  
  df %>% 
    select(order(colnames(.))) %>% 
    relocate(total_sites, .after = last_col()) %>% 
    gt() %>% 
    fmt_number(columns = c(2, 4, 6), 
               decimals = 0) %>% 
    cols_label(HTS_TST_POS_share = "(share)",
               TX_CURR_share = "", 
               TX_NEW_share = "", 
               facility_type = "Facility type") %>% 
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
          columns = c(2, 4, 6)
        )
      )
    ) %>% 
    tab_header(
      title = glue("{cntry}: MER SUMMARY BY FACILITY TYPE")
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: {metadata$source} & DATIM DAA Dataset | Ref id: {ref_id}"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() 
}                         



# PHC coverage summary table
pepfar_footprint_gt <- function(df){
  df %>%
    filter(merge_status == "merged") %>%
    distinct(orgunituid, merge_status_two, SA_FACILITY_TYPE) %>%
    calc_share(merge_status_two, SA_FACILITY_TYPE) %>%
    mutate(fac_type = fct_reorder(SA_FACILITY_TYPE, Share, .desc = T)) %>%
    arrange(fac_type, merge_status_two, Share) %>%
    gt_starter(., c(n, Share)) %>%
    tab_header(
      title = glue("{cntry} PEPFAR COVERAGE (HTS_POS, TX_CURR, TX_NEW) SUMMARY BY FACILITY TYPE")
    ) %>%
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM Data Alignment Activity Attribute Data 2023"))) %>%
    tab_options(
      source_notes.font.size = px(10)) %>%
    cols_hide(fac_type) %>%
    cols_label(SA_FACILITY_TYPE = "Facility Type",
               merge_status_two = "COVERAGE")
}

# PHC coverage summary table - pivoted
pepfar_footprint_gt_pivot <- function(df){
  df %>% 
    filter(merge_status == "merged") %>%
    distinct(orgunituid, merge_status_two, SA_FACILITY_TYPE) %>%
    count(SA_FACILITY_TYPE, merge_status_two, sort = T) %>%
    pivot_wider(names_from = "merge_status_two", values_from  = "n") %>% 
    mutate(total = `non-PEPFAR` + PEPFAR,
           total_2 = sum(total),
           pepfar_share = PEPFAR / total_2,
           non_pepfar_share = `non-PEPFAR` / total_2) %>% 
    mutate(pepfar_share = percent(pepfar_share, 1),
           non_pepfar_share = percent(non_pepfar_share, 1)) %>% 
    # mutate(`non-PEPFAR` = str_c(`non-PEPFAR`, " (",non_pepfar_share, ")")) %>% 
    # mutate(PEPFAR = str_c(PEPFAR, " (",pepfar_share, ")")) %>% 
    mutate(fac_type = fct_reorder(SA_FACILITY_TYPE, pepfar_share, .desc = T)) %>% 
    # arrange(fac_type, `non-PEPFAR`, non_pepfar_share, PEPFAR, pepfar_share) %>%
    select(-c(total, total_2)) %>% 
    # relocate(non_pepfar_share, .after = 2) %>% 
    # relocate(pepfar_share, .after = PEPFAR) %>%
    rename(`Non-PEPFAR Share` = non_pepfar_share,
           `PEPFAR Share` = pepfar_share,
           `PEPFAR Site Count` = PEPFAR,
           `Non-PEPFAR Site Count` = `non-PEPFAR`) %>% 
    gt() %>% 
    grand_summary_rows(columns = c(`Non-PEPFAR Site Count`, `PEPFAR Site Count`),
                       fns = list(
                         Total = ~sum(.)), 
                         formatter = fmt_number,
                         decimals = 0
    ) %>%
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() %>% 
    tab_header(
      title = glue("{cntry} SGAC DAA SUMMARY BY FACILITY TYPE")) %>%
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM Data Alignment Activity Attribute Data 2023"))) %>%
    tab_options(
      source_notes.font.size = px(10)) %>%
    cols_hide(fac_type) %>%
    cols_label(SA_FACILITY_TYPE = "Facility Type")
}


# Highlight certain rows for facility types
# Set a standardized formatting for highlighting
#' Title
#'
#' @param df 
#' @param rowvar column variable to be highlighted on 
#' @param fac_types type of facilities to highligh
#'
#' @return
#' @export
#'
#' @examples
row_highlight <- function(df, rowvar = SA_FACILITY_TYPE, 
                          fac_types = c("Health Post", "Primary Health Center")) {
  df %>%
    gt_highlight_rows(
      rows = {{ rowvar }} %in% fac_types,
      fill = grey10k,
      font_weight = 600,
      alpha = 0.45
    )
}


#' collapse facility type groups to new variable `fac_type`
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

collapse_fac_type <- function(df, unique_var = value) {
  df %>% 
    mutate(fac_type = case_when(
      {{unique_var}} %in% c("Dispensary/Pharmacy",
                            "Other Facility",
                            "Standalone Laboratory",
                            "Temporary Facility") ~ "Other Facility",
      is.na({{unique_var}}) ~ "Missing info",
      TRUE ~ {{unique_var}}
      )
    )
  }




#' Relevel facilty types when coercing to a factor
#'
#' @param df 
#' @param facility_type 
#'
#' @return
#' @export
#'
#' @examples

relevel_fac_type <- function(df, facility_type = fac_type){
  
  fac_order <- c("Primary Health Center", "Health Post", "Hospital", 
    "Mobile Health Clinic", "Other Facility", "Missing info", "Above site")
  
  df %>% 
    mutate(fac_type = fct_relevel({{facility_type}}, fac_order))
}

#' returns dataframe of site totals from DAA and PEPFAR site totals
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_ou_site_totals <- function(df) {
  
  df_totals <- df %>% 
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
    filter(indicator_type != "Not PEPFAR Supported") %>% 
    group_by(regionorcountry_name) %>%
    mutate(total_pepfar_sites = sum(n)) %>%
    ungroup() %>% 
    distinct(regionorcountry_name, total_sites, total_pepfar_sites)
  
  return(df_totals)
  
}


#' summary table by OUs starter code (table 2 and table 3)
#'
#' @param df 
#' @param viz_type type of summary table generated: `02_share_over_total` or `03_pepfar_share_hp_phc`
#'
#' @return
#' @export
#'
#' @examples
summary_tbl_ou_starter <- function(df, viz_type) {
  
  if (viz_type == "02_share_over_total") {
    pct_cols <- c(3,5,7)
    num_cols <- c(2,4,6)
    col_breaks <- c(2, 4, 6, 8)
    bold_col <- c(8)
  } else if (viz_type == "03_pepfar_share_hp_phc") {
    pct_cols <- c(4,7)
    num_cols <- c(2,3,5,6)
    col_breaks <- c(2, 5)
    bold_col <- c(2,6)
  }
  
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
    gt_theme_nytimes() %>% 
    # tab_header(
    #   title = glue("SHARE OF PRIMARY HEALTH CENTERS AND HEALTH POSTS SUPPORTED BY PEPFAR")
    # ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: DATIM DAA Site Attribute Data | Ref id: {ref_id}"))) %>% 
    tab_options(
      source_notes.font.size = px(10),
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

# Custom theme for tables

gt_theme_phc <- function (gt_object, ...) 
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  
  gt_object %>%
    tab_options(
      heading.align = "left", 
      #column_labels.border.top.style = "none",
      table.border.top.style = "none", 
      #column_labels.border.bottom.style = "none",
      column_labels.border.bottom.width = 1, 
      column_labels.border.bottom.color = "#334422",
      #table_body.border.top.style = "none", 
      table_body.border.bottom.color = "white",
      #heading.border.bottom.style = "none", 
      data_row.padding = px(7),
      column_labels.font.size = px(12), ...
    ) %>%
    tab_style(
      style = cell_text(
        color = "darkgrey",
        font = google_font("Source Sans Pro"), transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(
        color = grey60k,
        weight = "600",
        font = google_font("Source Sans Pro"), transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = everything())
    ) %>%
    tab_style(style = cell_text(
      font = google_font("Libre Franklin"),
      weight = 800
    ), 
    locations = cells_title(groups = "title")) %>%
    tab_style(style = cell_text(
      font = google_font("Source Sans Pro"),
      weight = 400
    ), 
    locations = cells_body()
    # ) %>% 
    # tab_style(style = cell_text(
    #   font = google_font("Source Sans Pro"),
    #   weight = 400
    # ), 
    # locations = cells_summary()
    ) 
}

