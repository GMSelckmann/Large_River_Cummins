library(tidyverse)
library(vegan)
library(MASS)
library(mmir)
#==============================================================================
hier.df <- Benthos::master %>% 
  clean_df() %>% 
  dplyr::select(final_id, phylum:species) %>% 
  distinct()
check_dups(hier.df, final_id)
hier.df %>% 
  dplyr::select(final_id, phylum) %>% 
  filter(is.na(phylum)) %>% 
  distinct() %>% 
  pull(final_id)
#------------------------------------------------------------------------------
jim.df <- data.table::fread("data/JIM_C_DATA_3.csv", data.table = FALSE) %>% 
  clean_df() %>% 
  mutate(final_id = stringr::str_replace_all(final_id, " ", "_"),
         final_id = case_when(
           final_id == "serratella_deficiens" ~ "teloganopsis_deficiens",
           final_id == "tvetenia_discoloripes" ~ "eukiefferiella_discoloripes",
           final_id == "turbellaria" ~ "trepaxonemata",
           final_id == "sphaeriidae" ~ "pisidiidae",
           final_id == "tubificidae" ~ "naididae",
           TRUE ~ final_id
         )) %>% 
  group_by_at(vars(-reporting_value)) %>% 
  summarize(reporting_value = sum(reporting_value)) %>% 
  ungroup()
#------------------------------------------------------------------------------
taxa.df <- left_join(jim.df, hier.df, by = "final_id") %>% 
 mmir::fill_taxa(final_id, phylum:species)
#------------------------------------------------------------------------------
taxa.df <- taxa.df %>% 
  mutate(date = as.Date(date, '%m/%d/%Y'),
         year = lubridate::year(date),
         color = case_when(
           year == 2012 ~ "#E69F00",
           year == 2013 ~ "#56B4E9",
           year == 2014 ~ "#009E73",
           TRUE ~ "ERROR"
         ))

rank.quo <- rlang::quo(genus)
gen.df <- taxa.df %>% 
  dplyr::select(event_id, year, color, !!rank.quo, reporting_value) %>% 
  dplyr::group_by(event_id, year, color, !!rank.quo) %>% 
  dplyr::summarize(reporting_value = sum(reporting_value)) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(!!rank.quo, reporting_value)
gen.df[is.na(gen.df)] <- 0

nmds <- metaMDS(gen.df[, 4:ncol(gen.df)], k = 3)
plot(nmds, dis = "sp", type = "n", xlim = c(-1,1), ylim = c(-0.8, 0.8)) 
points(nmds, display = "sites", cex = 0.8, pch=22)
ordiellipse(nmds, gen.df$year, kind = "sd", conf=0.95,
            draw = "polygon", col = gen.df$color, label = TRUE)
# ordihull(nmds, gen.df$year, display="sites", draw = "polygon", col=gen.df$color, label=TRUE)

