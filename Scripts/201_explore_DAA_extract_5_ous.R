# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Analyze zip file of site attribute data from select countries
# AUTHOR: Tim Essam | SI
# REF ID:   fbd7d896
# LICENSE: MIT
# DATE: 2023-03-23
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
library(googlesheets4)


# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))

# Unzip data in data folder of choice, purrr will iterate through and load  
zip_path <- "Data/Site Attrib Data/"

# Fetch full file paths for purr to read across  
site_attrib_ous <- list.files(path = zip_path, pattern = "2023-08-16",full.names = T)

# Extract names to use in naming list elements with set_names()
site_attrib_names <- sub(" data.*", "", list.files(path = zip_path, pattern = "2023-08-16"))

# Grab metadata
# get_metadata(file_path)

# REF ID for plots
ref_id <- "7853bd3e"

# LOAD DATA ============================================================================  

# Read in the the files, create a new column to track the OU and set the list 
# names for identification  
site_attrib_df <- map2(.x = site_attrib_ous,
                       .y = site_attrib_names,
                       ~vroom::vroom(.x) %>% mutate(operatingunit = .y)) 
names(site_attrib_df) <- set_names(site_attrib_names)

# Create a summary dataframe that can be used to make a facility count table
# for each OU
site_attrb_tbl <- map_dfr(site_attrib_df, ~.x %>% 
                            filter(!is.na(`Facility Type`)) %>% 
                            count(`Facility Type`, operatingunit) %>% 
                            mutate(`Facility Type` = ifelse(str_detect(`Facility Type`, "Dispens"), 
                                                  "Dispensary or Pharmacy", `Facility Type`))) %>% 
  arrange(`Facility Type`) %>% 
  spread(`Facility Type`, n)



# SUMMARY TABLE -----------------------------------------------------------

# Create a GT summary table
site_attrb_tbl %>%
  rowwise() %>% 
  mutate(total = sum(c_across(2:7), na.rm = T)) %>%
  mutate(operatingunit = ifelse(operatingunit == "CI", "Cote d'Ivoire", operatingunit)) %>% 
  arrange(desc(total)) %>% 
  gt() %>% 
  sub_missing(missing_text = ".",
  ) %>% 
  fmt_number(columns = is.numeric, 
             decimal = 0) %>% 
  gt_theme_nytimes() %>% 
  tab_header(
    title = glue("DAA FACILITY TYPE COUNTS BY OPERATING UNIT")
  ) %>% 
  tab_source_note(
    source_note = gt::md(glue("Source: DATIM DAA Site Attribute Data | Ref id: {ref_id}"))) %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  # Highlighting max value within each column
  gt_color_rows(columns = c(2:7), na.color = "white", 
                palette = c("#f7f7f7", scooter_med)) %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(8)
    )
  ) %>% 
  gtsave_extra(filename = glue("Images/August 2023/4_OU_DAA_summary_by_facility_type.png"))


# VIZ ============================================================================

# Summary viz
site_attrb_tbl %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "fac_type",
               values_to = "count") %>% 
  mutate(fac_order = tidytext::reorder_within(fac_type, count, operatingunit),
         ou_order = fct_reorder(operatingunit, count, .fun = sum, .na_rm = T),
         fac_type = ifelse(str_detect(fac_type, "Dispens"), "Dispensary or Pharmacy", fac_type)) %>%
  arrange(desc(ou_order)) %>% 
  ggplot(aes(y = ou_order, x = fac_type)) +
  geom_tile(aes(fill = count), color = grey20k) +
  geom_text(aes(label = comma(count), color = ifelse(count > 500, "white", grey90k)),
            family = "Source Sans Pro",
            size = 10/.pt) +
  scale_fill_gradient(na.value = "white", 
                      trans = "log",
                      labels = label_number_si(),
                      low = "#f7f7f7",
                      high = scooter_med) +
  scale_x_discrete(labels = label_wrap(10), position = "top") +
  si_style_nolines()  +
  scale_color_identity() +
  theme(legend.position = "none", 
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL,
       title = glue("DAA FACILITY TYPE COUNTS BY OPERATING UNIT"),
       caption = (glue("Source: DATIM DAA Site Attribute Data | Ref id: {ref_id}")))

si_save("Graphics/4_OU_DAA_summary_by_facility_type_2023_08_17.svg")


# SPINDOWN ============================================================================

site_attrib_df[["Malawi"]] <- site_attrib_df[["Malawi"]] %>% 
  mutate(`MOH Site ID` = as.character(`MOH Site ID`))

# Flatten the data into a single dataframe and save as an R file
site_attrib_df %>% 
  list_rbind() %>% 
  mutate(operatingunit = ifelse(operatingunit == "CI", "Cote d'Ivoire", operatingunit)) %>% 
  saveRDS(., file = "Dataout/site_attrib_df_5_ous")
