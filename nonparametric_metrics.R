#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 3/08/17
# Updated: 3/08/17
# Maintained: Zachary M. Smith
# Purpose: Calculate Macroinverterbate metrics for Jim Cummins large river 
#          data set.
# Output: A ".csv" file containing all of the calculated macroinvertebrate
#         metrics.
#==============================================================================
#==============================================================================
# Install the package devtools, which makes it easy to install the Benthos
# package.
install.packages(c("devtools","curl", "httr"))
# Install the Benthos package.
devtools::install_github("zsmith27/MMI", force = TRUE)
# Load the Benthos package.
library(MMI)
# Load the Vegan package.
library(vegan)
#==============================================================================
# Import the metrics table.
metrics.df <- read.csv("data/large_river_metrics.csv", stringsAsFactors = FALSE)
#==============================================================================
# Make sure any samples with a "D" designation are removed.
metrics.df <- metrics.df[!grepl("D", metrics.df$STATION_ID), ]
# Add some columns to make it easy to aggregate the data.
metrics.df$REACH <- substr(metrics.df$STATION_ID, 1, 2)
metrics.df$DATE <-  as.Date(metrics.df$DATE,'%m/%d/%Y')
metrics.df$YEAR <- as.numeric(format(metrics.df$DATE,'%Y'))
metrics.df$REACH_YEAR <- paste(metrics.df$REACH, metrics.df$YEAR, sep= "_")
#==============================================================================
# Reorganize the columns to make it easier to perform assessments.
org.cols <- c(1:7, (ncol(metrics.df) - 2):ncol(metrics.df), 8:(ncol(metrics.df) - 3))
metrics.df <- metrics.df[, org.cols]
#==============================================================================
# Convert aggregation columns to factors and assign the order you want the data
# and plots to appear in.
metrics.df$REACH <- factor(metrics.df$REACH, c("KX", "CR", "LF"))
metrics.df$REACH_YEAR <- factor(metrics.df$REACH_YEAR, c("KX_2012", "KX_2013", "KX_2014",
                                 "CR_2012", "CR_2013", "CR_2014",
                                 "LF_2012", "LF_2013", "LF_2014"))

metrics.df$STATION_ID <- factor(metrics.df$STATION_ID, c("KXvlUL_100A", "KXvlUL_100B", "KXvlUL_200C",
                                             "KXvlUR_100A", "KXvlUR_100B", "KXvlUR_200C",
                                             "KXvlLL_100A", "KXvlLL_100B", "KXvlLL_200C",
                                             "KXvlLR_100A", "KXvlLR_100B", "KXvlLR_200C",
                                             "CRckUL_100A", "CRckUL_100B", "CRckUL_200C",
                                             "CRckUR_100A", "CRckUR_100B", "CRckUR_200C",
                                             "CRckLL_100A", "CRckLL_100B", "CRckLL_200C",
                                             "CRckLR_100A", "CRckLR_100B", "CRckLR_200C",
                                             "LFUL_100A", "LFUL_100B", "LFUL_200C",
                                             "LFUR_100A", "LFUR_100B", "LFUR_200C",
                                             "LFLL_100A", "LFLL_100B", "LFLL_200C",
                                             "LFLR_100A", "LFLR_100B", "LFLR_200C"))
#==============================================================================
# Exclude any columns that are filled with all zeros.
ex.cols <- names(metrics.df[, 11:ncol(metrics.df)])[colSums(metrics.df[, 11:ncol(metrics.df)]) == 0]
metrics.df <- metrics.df[, !names(metrics.df) %in% ex.cols]
#==============================================================================

rich.df <- metrics.df[, 1:11]


vegan::rarecurve(rich.df$RICH, step = 20, sample = 2)

