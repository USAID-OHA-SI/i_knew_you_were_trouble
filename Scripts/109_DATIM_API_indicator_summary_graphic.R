# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Summary of API indicators pulled
# AUTHOR: Tim Essam | SI
# REF ID:   adf78f44
# LICENSE: MIT
# DATE: 2023-05-01
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
    

  # REF ID for plots
   ref_id <- "adf78f44"

  # Dataset for table
    df <- tibble::tribble(
               ~theme,      ~Indicator, ~fill_clr,
                "HSS",      "EMR_SITE",  "#cce4ff",
                "HSS",     "LAB_PTCQI",  "#cce4ff",
                "HSS",    "SC_ARVDISP",  "#cce4ff",
                "HSS",       "SC_CURR",  "#cce4ff",
         "Prevention",    "FPINT_SITE",  "#ffd5b5",
         "Prevention",      "GEND_GBV",  "#ffd5b5",
         "Prevention",        "KP_MAT",  "#ffd5b5",
         "Prevention",       "KP_PREV",  "#ffd5b5",
         "Prevention",      "OVC_SERV",  "#ffd5b5",
         "Prevention",       "PP_PREV",  "#ffd5b5",
         "Prevention",       "PREP_CT",  "#ffd5b5",
         "Prevention",      "PREP_NEW",  "#ffd5b5",
         "Prevention",       "TB_PREV",  "#ffd5b5",
         "Prevention",     "VMMC_CIRC",  "#ffd5b5",
            "Testing",     "CXCA_SCRN",  "#e5dcff",
            "Testing",     "HTS_INDEX",  "#e5dcff",
            "Testing",    "HTS_RECENT",  "#e5dcff",
            "Testing",      "HTS_SELF",  "#e5dcff",
            "Testing",       "HTS_TST",  "#e5dcff",
            "Testing",   "OVC_HIVSTAT",  "#e5dcff",
            "Testing",     "PMTCT_EID",  "#e5dcff",
            "Testing",      "PMTCT_FO",  "#e5dcff",
            "Testing", "PMTCT_HEI_POS",  "#e5dcff",
            "Testing",    "PMTCT_STAT",  "#e5dcff",
            "Testing",       "TB_STAT",  "#e5dcff",
          "Treatment",       "CXCA_TX",  "#ffe4b5",
          "Treatment",     "PMTCT_ART",  "#ffe4b5",
          "Treatment",        "TB_ART",  "#ffe4b5",
          "Treatment",       "TX_CURR",  "#ffe4b5",
          "Treatment",         "TX_ML",  "#ffe4b5",
          "Treatment",        "TX_NEW", "#ffe4b5",
          "Treatment",        "TX_RTT", "#ffe4b5",
          "Treatment",         "TX_TB", "#ffe4b5",
                "VLS",       "TX_PVLS",  "#ffa9ae"
         )



# PLOT DATA ============================================================================  

  df %>% 
    mutate(group_id = row_number(), .by = c(theme)) %>% 
    ggplot() +
    geom_tile(aes(x = theme, y = group_id, fill = fill_clr), color = "white") +
    geom_text(aes(x = theme, y = group_id, label = Indicator)) +
    scale_x_discrete(position = "top") +
    scale_y_reverse() +
    si_style_nolines() +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = "INDICATORS USED TO FLAG PEFAR SUPPORTED SITES THROUGH DATIM API",
         caption = glue("Source: DATIM API | Ref: {ref_id}")) +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_text(size = 14, family = "Source Sans Pro")) +
    coord_cartesian(expand = F) 
    
    si_save("Graphics/API_summary_indicators.svg")
  
