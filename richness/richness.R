library(tidyverse)
# devtools::install_github("zsmith27/mmir", ref = "dev", force = TRUE)
library(mmir)



master <- mmir::hier.fwmi %>% 
  toolbox::prep_df() %>% 
  fill_taxa(final_id, phylum:species)

# Jim's Data --------------------------------------------------------------
jim.df <- data.table::fread("data/large_river_cummins_taxa.csv") %>% 
  toolbox::prep_df() %>% 
  mutate(count_code = stringr::str_replace(station_id, ".*_", ""), 
         
         station = stringr::str_replace(station_id, "_.*", ""),
         reach = case_when(
           stringr::str_detect(station, "cr") ~ "cr",
           stringr::str_detect(station, "kx") ~ "kx",
           stringr::str_detect(station, "lf") ~ "lf",
           TRUE ~ "ERROR"
         ),
         all = "all",
         date = as.Date(date, "%m_%d_%Y"),
         year = lubridate::year(date),
         station_year = paste(station, year, sep = "_"),
         reach_year = paste(reach, year, sep = "_")
  ) %>% 
  left_join(master, by = "final_id") %>% 
  mutate(
    genus = case_when(
    family == "chironomidae" ~ "chironomidae", 
    family == "pisidiidae" ~ "pisidiidae",
    class == "oligochaeta" ~ "oligochaeta",
    phylum == "platyhelminthes" ~ "platyhelminthes",
    final_id == "serratella_deficiens" ~ "teloganopsis",
    final_id == "tvetenia_discoloripes" ~ "chironomidae",
    final_id == "sphaeriidae" ~ "pisidiidae",
    final_id == "turbellaria"  ~ "platyhelminthes",
    final_id == "tubificidae"  ~ "oligochaeta",
    TRUE ~ genus),
    genus_year = paste(genus, year, sep = "_"),
    genus_reach_year = paste(genus, reach, year, sep = "_")
  )

jim.df %>% 
  filter(is.na(genus))
# Aggregate ---------------------------------------------------------------
all.df <- data.frame(resolution = "all", stringsAsFactors = FALSE) %>% 
  mutate(
    abund = mmir::taxa_abund(jim.df, all, reporting_value,
                             genus),
    rich = mmir::taxa_rich(jim.df, all, genus)) %>% 
  rename(id = resolution)

reach.df <- jim.df %>% 
  dplyr::select(reach) %>% 
  distinct() %>% 
  mutate(
    abund = mmir::taxa_abund(jim.df, reach, reporting_value,
                             genus),
    rich = mmir::taxa_rich(jim.df, reach, genus)) %>% 
  rename(id = reach)

hundred.df <- jim.df %>% 
  select(unique_id, reach) %>% 
  distinct() %>% 
  mutate(
    abund = mmir::taxa_abund(jim.df, unique_id, reporting_value,
                             genus),
    rich = mmir::taxa_rich(jim.df, unique_id, genus)) %>% 
  rename(id = unique_id)

four.hundred.df <- jim.df %>% 
  select(station_year, reach) %>% 
  distinct() %>% 
  mutate(
    abund = mmir::taxa_abund(jim.df, station_year, reporting_value,
                             genus),
    rich = mmir::taxa_rich(jim.df, station_year, genus)) %>% 
  rename(id = station_year)

sixteen.hundred.df <- jim.df %>% 
  select(reach_year, reach) %>% 
  distinct() %>% 
  mutate(
    abund = mmir::taxa_abund(jim.df, reach_year, reporting_value,
                             genus),
    rich = mmir::taxa_rich(jim.df, reach_year, genus)) %>% 
  rename(id = reach_year)

counts.df <- bind_rows(hundred.df, four.hundred.df, sixteen.hundred.df) %>% 
  mutate(reach = factor(reach, levels = c("kx", "cr", "lf")))
total.counts.df <- bind_rows(all.df, reach.df)

# Plot --------------------------------------------------------------------
ggplot(counts.df, aes(abund, rich, group = reach, color = reach)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 1) +
  scale_color_manual(guide = guide_legend(title = "Reach"),
    values = c(
    "cr" = "#E69F00",
    "kx" = "#56B4E9",
    "lf" = "#009E73"
  ),
  labels = c(
    "cr" = "Carderock",
    "kx" = "Knoxville",
    "lf" = "Little Falls"
  )
  ) +
  geom_hline(aes(yintercept = all.df$rich), color = "red", size = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = reach.df[reach.df$id == "cr", "rich"]), size = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = reach.df[reach.df$id == "kx", "rich"]), size = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = reach.df[reach.df$id == "lf", "rich"]), size = 1, linetype = "dashed") +
  geom_label(aes(x = 20, y = all.df$rich,
                 label = paste0("Total Richness: All 3 Reaches (", all.df$rich, ")")),
             color = "red", vjust = "center", hjust = "inward") +
  geom_label(aes(x = 20, y = reach.df[reach.df$id == "cr", "rich"],
                 label = paste0("Total Richness: Carderock (",
                                reach.df[reach.df$id == "cr", "rich"], ")")),
             color = "black", vjust = "center", hjust = "inward") +
  geom_label(aes(x = 20, y = reach.df[reach.df$id == "kx", "rich"],
                 label = paste0("Total Richness: Knoxville (",
                                reach.df[reach.df$id == "kx", "rich"],")")),
             color = "black", vjust = "center", hjust = "inward") +
  geom_label(aes(x = 20, y = reach.df[reach.df$id == "lf", "rich"],
                 label = paste0("Total Richness: Little Falls (", 
                                reach.df[reach.df$id == "lf", "rich"], ")")),
             color = "black", vjust = "inward", hjust = "inward") +
  xlab("Abundance") +
  ylab("Richness") +
  theme_classic() +
  ylim(c(0, 120))

ggsave(paste0("rich_potomac_ms_", format(Sys.Date(), "%y_%m_%d"), ".png"),
       path = "richness/output",
       width = 7, height = 5, units = "in")  

# Rich Rank ---------------------------------------------------------------
year.df <- jim.df %>% 
  select(year, genus, reach) %>% 
  distinct() %>% 
  mutate(year = paste("year", year, sep = "_")) %>% 
  mutate(abund = mmir::taxa_abund(jim.df, genus_reach_year, reporting_value, genus)) %>% 
  spread(year, abund, fill = 0) %>% 
  mutate(count = year_2012 + year_2013 + year_2014) %>% 
  group_by(reach) %>% 
  mutate(rank = rank(-count, ties.method = "first")) %>% 
  ungroup() %>% 
  select(reach, rank, genus, count, year_2012, year_2013, year_2014) %>%
  mutate(genus = stringr::str_to_title(genus),
         reach = toupper(reach),
         reach = factor(reach, levels = c("KX", "CR", "LF"))) %>% 
  arrange(reach, rank) %>% 
  rename('2012' = "year_2012",
         "2013" = "year_2013",
         "2014" = "year_2014",
         Taxon = "genus") %>% 
  rename_all(stringr::str_to_title)

data.table::fwrite(year.df,
                   paste0("richness/output/abund_potomac_reach_year",
                          format(Sys.Date(), "%y_%m_%d"), ".csv"))

pct.all.df <- jim.df %>% 
  select(genus, reporting_value) %>% 
  group_by(genus) %>% 
  summarize(count = sum(reporting_value)) %>% 
  ungroup() %>% 
  mutate(pct = count / sum(count) * 100,
         pct = round(pct, 2),
         rank = rank(-pct, ties.method = "first")) %>% 
  select(rank, genus, pct) %>% 
  arrange(rank) %>% 
  mutate(pct = paste0(pct, "%"),
         genus = stringr::str_to_title(genus)) %>% 
  rename(Taxon = "genus",
         "Relative Abundance" = "pct") %>% 
  rename_all(stringr::str_to_title)

data.table::fwrite(pct.all.df,
                   paste0("richness/output/relative_abund_potomac_ms",
                          format(Sys.Date(), "%y_%m_%d"), ".csv"))
  
# pct.all.df %>% 
#   mutate(Taxon = factor(Taxon, levels = unique(Taxon))) %>% 
#   filter(Rank <= 10) %>% 
# ggplot(aes_string("Taxon", "Relative Abundance")) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 65, hjust = 1))

# NMDS --------------------------------------------------------------------
taxa.df <- jim.df %>% 
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
  select(unique_id, year, color, !!rank.quo, reporting_value) %>% 
  group_by(unique_id, year, color, !!rank.quo) %>% 
  summarize(reporting_value = sum(reporting_value)) %>% 
  ungroup() %>% 
  spread(!!rank.quo, reporting_value, fill = 0)
gen.df[is.na(gen.df)] <- 0


library(vegan)
set.seed(101582) #sample(1:10^6, 1)
nmds <- metaMDS(gen.df[, 5:ncol(gen.df)], k = 4, trymax = 1000)
# nmds <- metaMDS(gen.df[, 4:ncol(gen.df)][colSums(gen.df[, 4:ncol(gen.df)]) >= 500],
#                 k = 3, trymax = 1000)

png(file = paste0("richness/output/nmds_genus_potomac_",
                  format(Sys.Date(), "%y_%m_%d"), ".png"),
    bg = "white")
take  <- c(1, 2)

plot(nmds, choices = take,  dis = "sp", type = "n", xlim = c(-1,1), ylim = c(-0.8, 0.8)) 
#plot(nmds,  dis = "sp", type = "n", xlim = c(-1,1), ylim = c(-0.8, 0.8)) 
points(nmds, choices = take, display = "sites", cex = 0.8, pch=22)
ordiellipse(nmds,choices = take, gen.df$year, kind = "sd", conf=0.95,
            draw = "polygon", col = gen.df$color, label = FALSE)
legend(0.6, 0.9, legend = c("2012", "2013", "2014"),
       col=c("#E69F00", "#56B4E9", "#009E73"), lty="solid", lwd = 10)
dev.off()
 # ordihull(nmds, gen.df$year, display="sites", draw = "polygon", col=gen.df$color, label=TRUE)
