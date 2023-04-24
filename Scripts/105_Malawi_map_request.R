# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Create map of Malawi Health Centers in health posts in color coded by PEPFAR supported vs non-supported
# AUTHOR: Tim Essam | SI
# REF ID:   fbd7d896
# LICENSE: MIT
# DATE: 2023-03-23
# NOTES: Tim Essam | SI


# SETUP  --------------------------------------------------------------------

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(gisr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(gt)
  library(gtExtras)
  library(googlesheets4)
  library(googledrive)

  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  shpdata <- file.path(glamr::si_path("path_vector")) # for shapefiles
  latdata <- list.files(file.path(shpdata, "OU-sites"), pattern = "Malawi", full.names = T)
  rasdat <- glamr::si_path("path_raster")
  
  # Sites pulled via API by AC
  ss_sites <- "1qX_qzyaS4eG9ivGAyoop5I2uPvzLOjwy"
  
  # Use google drive to download file and then pass temp path through to vroom to open
  temp <- tempfile(fileext = ".zip")
  dl <- drive_download(as_id(ss_sites), path = temp, overwrite = T)
  
  source("Scripts/00_helper_functions.R")

# LOAD DATA ---------------------------------------------------------------

  df_daa_sa <- readRDS("Dataout/site_attrib_df") %>% 
    filter(operatingunit == "Malawi",
           dataelement == "SA_FACILITY_TYPE") 

  df_daa <- readRDS("Dataout/daa_fac_df") %>% 
    filter(regionorcountry_name == "Malawi")

  # Not used 
  # df_msd <- readRDS("Dataout/msd_site_dfs") %>% 
  #   filter(operatingunit == "Malawi")
  
  df_api <- vroom::vroom(dl$local_path) %>% 
    filter(operatingunit %in% "Malawi", sitetype == "Facility") 

  df_api_tagged <- 
    df_api %>% select(operatingunit:indicatortype) %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = indicatortype, values_from = value) %>% 
    mutate(pepfar_fp = case_when(
      is.na(DSD) & is.na(TA) ~ "Non-PEPFAR",
      TRUE ~ "PEPFAR"
    )) %>% 
    arrange(pepfar_fp) %>% prinf()

  # GIS data
  grabr::get_outable() %>% filter(operatingunit == "Malawi")
  
  cntry <- "Malawi"
  spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  mwi_geo <- purrr::map(list(3, 6), ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                       level = .x))
  names(mwi_geo) <- list("adm0", "psnu")
  
  # Breaking change to extract_facilities required modification in workflow (issues submitted in gisr)
  df_locs_mwi <- gisr::extract_locations(cntry, level = 7) %>% 
    extract_facilities()


# MUNGE -------------------------------------------------------------------

  map(list(df_api_tagged, df_daa, df_daa_sa, df_locs_mwi), ~names(.x))
  
  # Perfect match between DAA and geospatial (pulling from same source)
  df_map <- 
    df_daa %>% 
    tidylog::left_join(., df_locs_mwi, by = c("orgunit_internal_id" = "id")) %>% 
    tidylog::left_join(., df_daa_sa, by = c("orgunit_internal_id" = "orgunit")) %>% 
    mutate(merge_attributes = ifelse(is.na(dataelement), "DAA only", "DAA & Attrb")) %>% 
    tidylog::left_join(., df_api_tagged, by = c("orgunit_internal_id" = "orgunituid")) %>% 
    mutate(merge_api = ifelse(is.na(sitetype), "DAA only", "DAA & api"))

  
#  MAP TO SHOW HEALTH CENTERS AND HEALTH POSTS  ---------------------------
  
  
  # GT SUMMARY TABLES OR COORDINATE AVAILABILITY
  df_map %>%
    mutate(
      has_coord =
        case_when(
          is.na(latitude) & is.na(longitude) ~ "missing",
          TRUE ~ "available"
        ),
      fac_type = case_when(
        str_detect(value, "Health Post|Primary") ~ "Health Center or Post", 
        TRUE ~ "Other Facilties"
      ),
      pepfar_fp = ifelse(is.na(pepfar_fp), "Non-PEPFAR", pepfar_fp)
    ) %>%
    count(fac_type, pepfar_fp, has_coord) %>% 
    spread(has_coord, n) %>% 
    mutate(total = available + missing,
           complete = available / total) %>% 
    gt(groupname_col = "fac_type") %>% 
    fmt_percent(columns = complete, 
                decimals = 0) %>% 
    gtExtras::gt_theme_nytimes() %>% 
    cols_label(pepfar_fp = "",
               available = "Has Coordinates",
               complete = "% with Coordinates") %>% 
    grand_summary_rows(columns = c(available, missing, total),
                       fns = list(
                         Total = ~sum(.)), 
                       formatter = fmt_number,
                       decimals = 0
                       ) %>% 
  tab_options(
    column_labels.font.size = px(15),
    row_group.font.weight = "bold"
  ) %>% 
    tab_header(
      title = glue::glue("OVER 20% OF FACILITIES ARE MISSING SITE COORDINATES IN MALAWI")
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue::glue("DATIM Data Alignment Activity Attribute Data & DATIM API 2023"))
      ) %>% 
    row_highlight(., rowvar = pepfar_fp, fac_types = "PEPFAR") %>% 
 gtsave_extra(filename = glue("Images/MWI_coordinate_summary.png"))
  

# Create dataframe for map  
  df_map_fac <- df_map %>% 
    mutate(fac_type = case_when(
      str_detect(value, "Health Post|Primary") ~ "Health Center or Post", 
      TRUE ~ "Other"
    ),
    pepfar_hp = case_when(
      fac_type == "Health Center or Post" & pepfar_fp == "PEPFAR" ~ "Health Center or Post \nPEPFAR",
      fac_type == "Health Center or Post" & is.na(pepfar_fp) ~ "Health Center or Post \nNON-PEPFAR",
      fac_type == "Other" & pepfar_fp == "PEPFAR" ~ "Other Facilities \nPEPFAR",
      TRUE ~ "Other Facilities \n NON-PEPFAR    "
    ))

  # CREATE MAPS
  terrain_map(countries = mwi_geo$adm0,
              adm0 = mwi_geo$adm0,
              terr = glamr::si_path("path_raster"), 
              mask = T) +
    geom_point(data = df_map_fac, aes(x = longitude, y = latitude, color = pepfar_hp)) +
    scale_color_manual(values = si_palettes$old_rose[1:4]) +
    facet_wrap(~pepfar_hp, nrow = 1) +
    si_style_map() +
    labs(x = NULL, y = NULL, 
         title = "PEPFAR SUPPORTS OVER 97% (524/537) HEALTH CENTERS OR POSTS IN MALAWI",
         subtitle = "PEPFAR support defined as TA or DSD services provided at a facility",
         caption = "DATIM Data Alignment Activity Attribute Data & DATIM API 2023") +
    theme(legend.position = "none")  +
    theme(
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 12,
        color = "white", fill = scooter, box.color = grey90k,
        halign = 0.5, linetype = 1, r = unit(7, "pt"), width = unit(1, "npc"),
        padding = margin(5, 5, 5, 5), margin = margin(3, 3, 3, 3)
      )
    )
  
  si_save("Graphics/MWI_PEPFAR_footprint_by_site_attribute.svg")
  


  