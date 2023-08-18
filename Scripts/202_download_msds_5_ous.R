# PROJECT: Analysis of Site level data on PHC
# PURPOSE: Pull site level MSD from PANO into MSD folder
# AUTHOR: Tim Essam | SI
# REF ID:   0062fc80
# LICENSE: MIT
# DATE: 2023-04-13
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(gagglr)
library(grabr)
library(fs)
library(glue)

# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))

# Create a MSD folder within the Data folder to store MSDs in  
dir_msds <- "Data/MSD"
dir.create(dir_msds)

# Global objects
indic_list <- c("PrEP_NEW", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_PVLS")

# ESTABLISH SESSION ============================================================================  

sess <- pano_session(username = pano_user(), password = pano_pwd())

url <- "https://pepfar-panorama.org/forms/downloads/"


# IDENTIFY PERIOD

recent_fldr <- url %>%
  pano_content(session = sess) %>%
  pano_elements() %>%
  filter(str_detect(item, "^MER")) %>%
  pull(item)

curr_status <- ifelse(str_detect(recent_fldr, "Clean"), "clean", "initial")
curr_fy <- str_extract(recent_fldr, "[:digit:]{4}") %>% as.numeric()
curr_qtr <- str_extract(recent_fldr, "(?<=Q)[:digit:]") %>% as.numeric()

items <- map2_dfr(c("mer", "mer", "financial"),
                  c(TRUE, FALSE, FALSE),
                  ~ pano_extract(item = .x,
                                 unpack = .y))


# Concatenated list of OUs for analysis
ou_list <- str_c("Kenya", "Malawi", "Nigeria", "West Africa Region", "Cote d'Ivoire", sep = "|")

regex_ous <- glue("MER_Structured_Datasets_Site_IM_FY21-23.*{ou_list}\\.zip") %>% as.character()

#download site level MSDs to data folder
items %>%
  filter(parent == "MER FY2023 Q2 Clean/Site Level") %>% 
  filter(str_detect(item, regex_ous)) %>%
  distinct(path) %>%
  pull(path) %>%
  walk(~pano_download(item_url = .x,
                      session = sess,
                      dest = dir_msds))

# RENAME MSDs ============================================================================

# Some of the MSDs come down with special characters in them
# These need to be escaped when regular expression matching and removed

list.files(path = dir_msds, 
           pattern = "\\%20", 
           full.names = T) %>% 
  walk(~file.rename(.x, 
                    stringr::str_replace_all(.x, "\\%20", " ") %>% 
                      stringr::str_replace_all("\\%27", "'")
  )
  )


# COMBINE MSDs & FILTER DOWN ----------------------------------------------

msd_site_df <- list.files(path = dir_msds, full.names = TRUE) %>% 
  map(.f = \(x) read_psd(x) %>% 
        filter(indicator %in% indic_list), .progress = TRUE) %>%
  list_rbind()

msd_ghana <- msd_site_df %>% 
  filter(operatingunit == "West Africa Region",
         country == "Ghana")

msd_site_df_final <- msd_site_df %>% 
  filter(operatingunit != "West Africa Region") %>% 
  rbind(msd_ghana)

# save as an Rds file
saveRDS(msd_site_df_final, file = "Dataout/msd_site_dfs_5_ous") 
