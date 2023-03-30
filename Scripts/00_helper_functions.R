#Helper functions for the project

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
  