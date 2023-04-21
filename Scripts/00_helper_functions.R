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


create_phc_gt <- function(df){
  df %>% 
    select(order(colnames(.))) %>% 
    relocate(total_sites, .after = 8) %>% 
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
                         Total = ~sum(.))
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

