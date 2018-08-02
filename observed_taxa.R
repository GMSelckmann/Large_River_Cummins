library(tidyverse)
final.df <- data.table::fread("data/large_river_cummins_taxa.csv",
                            data.table = FALSE) %>% 
  rename_all(tolower) %>% 
  mutate(Taxon = paste0(toupper(substr(final_id, 1, 1)),
                       substr(tolower(final_id), 2, nchar(final_id))),
         Reach = stringr::str_sub(station_id, 1, 2),
         Reach = case_when(
           Reach == "CR" ~ "Carderock",
           Reach == "KX" ~ "Knoxville",
           Reach == "LF" ~ "Little Falls",
           TRUE ~ "ERROR"
         ),
         date = as.Date(date, "%m/%d/%Y"),
         year = lubridate::year(date)) %>% 
  select(Reach, year, Taxon, reporting_value) %>% 
  group_by(Reach, year, Taxon) %>% 
  summarize(count = sum(reporting_value)) %>% 
  ungroup() %>% 
  spread(year, count) %>% 
  replace_na(list("2012" = 0, "2013" = 0, "2014" = 0)) %>% 
  mutate(Overall = select(., "2012", "2013", "2014") %>% rowSums()) %>% 
  arrange(Reach, desc(Overall)) %>% 
  select(Reach, Taxon, Overall, "2012", "2013", "2014")



file.dir <- file.path("output", paste0("taxa_observed_", Sys.Date(), ".csv"))
data.table::fwrite(final.df, file.dir)
