#Helper functions for the project

# Calculates the share for types of facilities
calc_share <- function(df, ...){
  df %>% 
    count(..., sort = T) %>% 
    mutate(Share = n / sum(n))
}
