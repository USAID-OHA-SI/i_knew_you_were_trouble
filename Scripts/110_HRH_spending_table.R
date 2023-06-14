# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   4210595f 
# LICENSE:  MIT
# DATE:     2023-06-08
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(gtExtras)
  library(gt)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  ref_id <- "4210595f"

# IMPORT ------------------------------------------------------------------
  
  df_hrh <- readRDS("Dataout/df_hrh_fac_phc.RDS") 
  

# MUNGE -------------------------------------------------------------------
  
total_spend <-  df_hrh %>% 
    # pivot_daa() %>% 
    filter(dataelement == "SA_FACILITY_TYPE") %>%
    #  mutate(fac_type = collapse_2())
    collapse_fac_type(., unique_var = value) %>%
    group_by(regionorcountry_name, fac_type) %>% 
    summarise(across(starts_with("actual_annual_spend"), sum, na.rm = T), .groups = "drop") %>% 
    group_by(regionorcountry_name) %>% 
    summarise(across(starts_with("actual_annual_spend"), sum, na.rm = T), .groups = "drop") %>% 
    rename(total_spend = actual_annual_spend)
    
 hrh_table <-  df_hrh %>% 
    # pivot_daa() %>% 
    filter(dataelement == "SA_FACILITY_TYPE") %>%
    #  mutate(fac_type = collapse_2())
    collapse_fac_type(., unique_var = value) %>%
    group_by(regionorcountry_name, fac_type) %>% 
    summarise(across(starts_with("actual_annual_spend"), sum, na.rm = T), .groups = "drop") %>% 
   #mutate(actual_annual_spend = )
    pivot_wider(names_from = "fac_type", values_from  = "actual_annual_spend") %>% 
    left_join(total_spend, by = c("regionorcountry_name")) %>% 
    replace(is.na(.), 0) %>% 
    mutate(phc_share = `Primary Health Center`/total_spend,
           health_post_share = `Health Post` / total_spend,
           hospital_share = Hospital / total_spend,
           other_share = `Other Facility` / total_spend,
           mobile_share = `Mobile Health Clinic`/total_spend) %>% 
    arrange(desc(phc_share)) %>% 
    select(regionorcountry_name, `Primary Health Center`,phc_share, `Health Post`, health_post_share,
           `Mobile Health Clinic`, mobile_share, Hospital, hospital_share, `Other Facility`, other_share, total_spend) %>% 
  mutate(`Health Post` = ifelse(`Health Post` == 0, NA, `Health Post`),
         `Mobile Health Clinic` = ifelse(`Mobile Health Clinic` == 0, NA, `Mobile Health Clinic`))

 # VIZ -----------------------------------------------------------------------------
 hrh_table %>%
   rename(Country = regionorcountry_name) %>% 
   hrh_table_starter() %>% 
   cols_label(phc_share = "(share)",
              health_post_share = "", 
              mobile_share = "", 
              hospital_share = "",
              other_share = "",
              total_spend = "Total spending") %>% 
   tab_row_group(
     label = "Smaller TX OUs",
     rows = Country %ni% c("Mozambique", "South Africa", "Zimbabwe",
                           "Zambia", "Uganda", "Malawi")
   ) %>%
   tab_row_group(
     label = "Larger TX OUs",
     rows = Country %in% c("Mozambique", "South Africa", "Zimbabwe",
                           "Zambia", "Uganda", "Malawi")
   ) %>% 
   tab_header(
     title = glue("ANNUAL SPENDING ON HRH STAFFING (SALARIES AND FRINGE) BY OU AND FACILITY TYPE, 2022")
   ) %>% 
   gt_color_rows(columns = c(2,4,6), na.color = "white", 
                 palette = c("#f7f7f7", moody_blue)) %>%
   drkn_clmn_hdr() %>% 
   gtsave_extra(filename = glue("Images/table_hrh_spending.png"))  
 
 
 
 

 
