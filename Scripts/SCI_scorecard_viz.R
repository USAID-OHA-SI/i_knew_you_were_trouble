# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  sci viz
# REF ID:   06c7ab65 
# LICENSE:  MIT
# DATE:     2023-04-05
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
  library(rcartocolor)
  library(ggnewscale)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
  ref_id <- "06c7ab65"
  
  g_id <- "1-nzn8jqmyeW74Oh5vNugiSeLoRi2jGQpJ0M3Ar23nKQ"
  
  source_note <- "WHO Global Health Observatory Indicator Metadata Registry List"

# IMPORT ------------------------------------------------------------------
  
df_sci <- read_sheet(g_id)

# MUNGE -------------------------------------------------------------------
  
df_viz <- df_sci %>% 
    filter(country %in% c("Zimbabwe",
                          "Rwanda",
                          "Botswana",
                          "Malawi",
                          "Eswatini",
                          "Burundi",
                          "Namibia",
                          "Zambia",
                          "Nigeria",
                          "Lesotho",
                          "Thailand"
                          )) %>% 
    select(country, uhc_service_coverage_index, uhc_subindex1_capacity_access, uhc_subindex4_id,  year) %>% 
    pivot_longer(cols = c(2:4), names_to = "indicator") %>% 
    mutate(fill_color = genoa,
           indicator = case_when(indicator == "uhc_service_coverage_index" ~ "UHC Service Coverage Index",
                                 indicator == "uhc_subindex1_capacity_access" ~ "UHC Subindex: Capacity Access",
                                 indicator == "uhc_subindex4_id" ~ "UHC Subindex: Infectious Disease"))

#VIZ -----------------------------------------------------------------------------
  
  df_viz %>%
    ggplot(aes(indicator, country,
               fill = fill_color, color = fill_color), shape = 21) +
    geom_point(size = 6.5) +
    # geom_text(
    #           vjust = .5, hjust = .5,
    #           aes(label = value), family = "Source Sans Pro SemiBold", size = 3) +
    # geom_text(data = . %>% filter(achv == TRUE & indicator != "Epi\nControl"), 
    #           vjust = .5, hjust = .5,
    #           aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 3) +
    # geom_text(data = . %>% filter(achv != TRUE & indicator == "Epi\nControl"), 
    #           vjust = .5, hjust = .5,
    #           aes(label = value), family = "Source Sans Pro SemiBold", size = 2.5) +
    geom_text(vjust = .5, hjust = .5,
              aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 2.5) +
    # geom_text(size = 3, nudge_x = 1, na.rm = TRUE,
    #           aes(label = lab_epi), color = matterhorn, family = "Source Sans Pro") +
    #facet_grid(grouping~., scales = "free_y", space = "free_y") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_shape_identity() +
    scale_x_discrete(position = "top", expand = c(.05, .05)) +
    scale_y_reordered() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         caption = glue("Source: {source_note} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          strip.text.y = element_blank(),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/sci_scorecard.svg")
  

# POTENTIAL OPTION FOR ENCODING INDICES WITH COLOR ------------------------

  ou_sort_order <- c("Zimbabwe", "Rwanda", "Botswana", "Malawi",
                       "Eswatini", "Burundi", "Namibia","Zambia",
                       "Nigeria", "Lesotho", "Thailand")
  
  # Alternative option with a gradient fill for the scores
  df_viz %>%
    mutate(country = fct_relevel(country, rev(ou_sort_order))) %>% 
    ggplot(aes(indicator, country,
               color = value), shape = 21) +
    geom_point(size = 9) +
    geom_point(size = 9, stroke = .9, shape = 1, color = grey90k) +
    scale_color_carto_c(palette = "TealGrn") +
    ggnewscale::new_scale_color() +
    geom_text(vjust = .5, hjust = .5,
              aes(label = value, 
                  color = ifelse(value < 50, grey90k, "white")),
              family = "Source Sans Pro SemiBold", 
              size = 10/.pt) +
    scale_color_identity() +
    scale_x_discrete(position = "top", expand = c(.05, .05)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         caption = glue("Source: {source_note} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          strip.text.y = element_blank(),
          panel.spacing.y = unit(.5, "lines"),
          legend.position = "none")
  
  
  
  