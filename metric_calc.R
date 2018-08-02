#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 3/08/17
# Updated: 7/25/2018
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
# Install and load the Benthos package
#==============================================================================
# Install the Benthos package.
#devtools::install_github("zsmith27/Benthos", force = TRUE)
# Load the Benthos package.
library(Benthos)
# Load the Master Taxa List contained within the Benthos package.
data(master)
# Load the Vegan package.
library(vegan)
# Load the Tidyverse packages.
library(tidyverse)
#==============================================================================
# Import the metrics table.
reach.df <- read.csv("data/reach_year_agg.csv", stringsAsFactors = FALSE) %>% 
  toolbox::standard_char() %>% 
  mutate(
    GENUS = case_when(
      FAMILY == "chironomidae" ~ "chironomidae", 
      FAMILY == "pisidiidae" ~ "pisidiidae",
      CLASS == "oligochaeta" ~ "oligochaeta",
      PHYLUM == "platyhelminthes" ~ "platyhelminthes",
      FINAL_ID == "serratella_deficiens" ~ "teloganopsis",
      FINAL_ID == "tvetenia_discoloripes" ~ "chironomidae",
      FINAL_ID == "sphaeriidae" ~ "pisidiidae",
      FINAL_ID == "turbellaria"  ~ "platyhelminthes",
      FINAL_ID == "tubificidae"  ~ "oligochaeta",
      TRUE ~ GENUS)
  )
  
wide.df <- Benthos::wide(reach.df, "GENUS")
#vegan::rarecurve(wide.df[, 8:ncol(wide.df)], step = 20)
#==============================================================================
source("large_river_functions.R")
#==============================================================================
samp.size = 1000
#==============================================================================
# Carderock Samples
system.time(rare.CR_2012 <- prep_rare(wide.df, "cr_2012", samp.size))
rare.CR_2013 <- prep_rare(wide.df, "cr_2013", samp.size)
rare.CR_2014 <- prep_rare(wide.df, "cr_2014", samp.size)
cr.reach <- rbind(rare.CR_2012, rare.CR_2013, rare.CR_2014)
# Knoxville Samples
rare.KX_2012 <- prep_rare(wide.df, "kx_2012", samp.size)
rare.KX_2013 <- prep_rare(wide.df, "kx_2013", samp.size)
rare.KX_2014 <- prep_rare(wide.df, "kx_2014", samp.size)
kx.reach <- rbind(rare.KX_2012, rare.KX_2013, rare.KX_2014)
# Little Falls Samples
rare.LF_2012 <- prep_rare(wide.df, "lf_2012", samp.size)
rare.LF_2013 <- prep_rare(wide.df, "lf_2013", samp.size)
rare.LF_2014 <- prep_rare(wide.df, "lf_2014", samp.size)
lf.reach <- rbind(rare.LF_2012, rare.LF_2013, rare.LF_2014)

all.reach <- list(cr.reach, kx.reach, lf.reach)
#==============================================================================
# Calculate Metrics
#==============================================================================
# Calculate metrics for each reach.
metrics.list <- lapply(all.reach, function(x){
  large_river_metrics(x, master)
})
#==============================================================================
# Create a list of the metrics calculated
metrics.vec <- names(metrics.list[[1]][, 9:ncol(metrics.list[[1]])])
#==============================================================================