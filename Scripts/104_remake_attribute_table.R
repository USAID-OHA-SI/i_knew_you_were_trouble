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
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    source("Scripts/00_helper_functions.R")

      
  # Objects
   source <- "S/GAC Site Attribute Team"
   gs_id <- "1eutwJ64LpckbhO9grSKgHKH5MPKvM-VWrrEB07qPQ2Y"
   
  # REF ID for plots
    ref_id <- "2d8b2324"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  da_tbl <- googlesheets4::read_sheet(gs_id)

# TABLE CREATION ============================================================================
  
  # Create a highlighted attribute table focusing on Botswana and Malawi  
  da_tbl %>% 
      gt() %>% 
      fmt_number(columns = 3:4,
                 decimals = 0) %>% 
      fmt_percent(columns = 5, 
                  decimals = 1) %>% 
      gt_theme_nytimes() %>% 
      sub_missing(missing_text = ".") %>% 
      cols_align(columns = 2,
                 align = c("right")) %>% 
      cols_label(`Share of facilities with attributes` = "Completeness") %>% 
      # gt_color_rows(columns = 5, 
      #                   na.color = "white", 
      #                   palette = c("#f7f7f7", scooter_med)) %>% 
      gt_highlight_rows(
        rows = Country %in% c("Botswana", "Malawi"),
        fill = scooter_med,
        font_weight = 600,
        alpha = 0.25
      ) %>% 
      gt::tab_header(
        title = glue::glue("SITE ATTRIBUTE COMPLETENESS")
      ) %>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("{source} | Ref id: {ref_id}"))) %>% 
      gt::tab_options(
        source_notes.font.size = gt::px(10),
        column_labels.font.size = px(15)) %>% 
    gtsave_extra(filename = glue("Images/Site_attibute_completeness_table.png"), vwidth = 800)
      
  
# VIZ ============================================================================
    
    #facilities covered in round two analysis
    da_tbl %>% 
      gt() %>% 
      fmt_number(columns = 3:4,
                 decimals = 0) %>% 
      fmt_percent(columns = 5, 
                  decimals = 1) %>% 
      gt_theme_nytimes() %>% 
      sub_missing(missing_text = ".") %>% 
      cols_align(columns = 2,
                 align = c("right")) %>% 
      cols_label(`Share of facilities with attributes` = "Completeness") %>% 
      # gt_color_rows(columns = 5, 
      #                   na.color = "white", 
      #                   palette = c("#f7f7f7", scooter_med)) %>% 
      gt_highlight_rows(
        rows = Country %in% ou_list,
        fill = grey10k,
        font_weight = 600,
        alpha = 0.45
      ) %>% 
      gt::tab_header(
        title = glue::glue("SITE ATTRIBUTE COMPLETENESS")
      ) %>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("{source} | Ref id: {ref_id}"))) %>% 
      gt::tab_options(
        source_notes.font.size = gt::px(10),
        column_labels.font.size = px(15)) %>% 
      gtsave_extra(filename = glue("Images/Site_attibute_completeness_rnd2_table.png"), vwidth = 800)

# site count ============================================================================

    # What are the site counts and share
    da_tbl %>% 
      filter(Country %in% ou_list) %>% 
      summarize(tot = sum(`Total Facilities`, na.rm = T),
                tot_atrb = sum(`Facilities with attributes`),
                completeness = tot_atrb / tot)





