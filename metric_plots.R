#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 3/08/17
# Updated: 3/08/17
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
# Install and load the Benthos package
#==============================================================================
# Install the Benthos package.
devtools::install_github("zsmith27/Benthos", force = TRUE)
# Load the Benthos package.
library(Benthos)
# Load the Master Taxa List contained within the Benthos package.
data(master)
# Load the Vegan package.
library(vegan)
#==============================================================================
# Import the metrics table.
reach.df <- read.csv("data/reach_year_agg.csv", stringsAsFactors = FALSE)
wide.df <- Benthos::wide(reach.df, "GENUS")
#vegan::rarecurve(wide.df[, 8:ncol(wide.df)], step = 20)
#==============================================================================
source("large_river_functions.R")
#==============================================================================
# Carderock Samples
rare.CR_2012 <- prep_rare(wide.df, "CR_2012", 10)
rare.CR_2013 <- prep_rare(wide.df, "CR_2013", 10)
rare.CR_2014 <- prep_rare(wide.df, "CR_2014", 10)
cr.reach <- rbind(rare.CR_2012, rare.CR_2013, rare.CR_2014)
# Knoxville Samples
rare.KX_2012 <- prep_rare(wide.df, "KX_2012", 10)
rare.KX_2013 <- prep_rare(wide.df, "KX_2013", 10)
rare.KX_2014 <- prep_rare(wide.df, "KX_2014", 10)
kx.reach <- rbind(rare.KX_2012, rare.KX_2013, rare.KX_2014)
# Little Falls Samples
rare.LF_2012 <- prep_rare(wide.df, "LF_2012", 10)
rare.LF_2013 <- prep_rare(wide.df, "LF_2013", 10)
rare.LF_2014 <- prep_rare(wide.df, "LF_2014", 10)
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
# For each of the metrics calculate the min and max value observed for all 
# reaches and years.
metric.ranges <- lapply(metrics.vec, function(x){
  ranges <- lapply(metrics.list, function(y){
    range.df <- data.frame(METRIC = x)
    range.df$MIN <- min(y[, x])
    range.df$MAX <- max(y[, x])
    return(range.df)
  })
  bind.range <- do.call(rbind, ranges)
  range.df <- data.frame(METRIC = x)
  range.df$MIN <- round(min(bind.range[, "MIN"])) 
  range.df$MAX <- round(max(bind.range[, "MAX"]))
  return(range.df)
})
# Create a data frame of the metric ranges (min-max).
final.ranges <- do.call(rbind, metric.ranges)
#==============================================================================
# For each metric generate the 3 plots for each reach, join the plots, and
# export to the output folder.
lapply(metrics.vec, function(x){
  plot.list <- lapply(metrics.list, function(y){
    keep.cols <- c("UNIQUE_ID", "STATION_ID", "AGENCY_CODE", "DATE", "METHOD",
                   "SAMPLE_NUMBER", "CONDITION", "SAMPLE_COUNT", x)
    metrics.df <- y[, keep.cols]
    add.title <- ifelse(grepl("CR", metrics.df[1, "STATION_ID"]), "Carderock",
                        ifelse(grepl("KX", metrics.df[1, "STATION_ID"]), "Knoxville",
                               ifelse(grepl("LF", metrics.df[1, "STATION_ID"]),
                                      "Little Falls", "ERROR")))
    add.yaxis.title <- ifelse("RICH" %in% names(metrics.df)[9], "Richness",
                              ifelse("RICH_EPHEMEROPTERA" %in% names(metrics.df)[9], "EPhemeroptera Richness",
                                     ifelse("RICH_TRICHOPTERA" %in% names(metrics.df)[9], "Trichoptera Richness",
                                            ifelse("RICH_DIPTERA" %in% names(metrics.df)[9], "Diptera Richness",
                                                   ifelse("PCT_EPHEMEROPTERA" %in% names(metrics.df)[9], "Percentage of Ephemeroptera",
                                                          ifelse("PCT_TRICHOPTERA" %in% names(metrics.df)[9], "Percentage of Trichoptera",
                                                                 ifelse("PCT_TANYTARSINI" %in% names(metrics.df)[9], "Percentage of Tanytarsini",
                                                                        ifelse("PCT_NON_TANYTARSINI_NON_INSECTA" %in% names(metrics.df)[9], "Percentage of Non-Tanytarsini and Non-Insecta",
                                                                               ifelse("PCT_EPT" %in% names(metrics.df)[9], "Percentage of EPT",
                                                                                      ifelse("PCT_PLECOPTERA" %in% names(metrics.df)[9], "Percentage of Plecoptera",
                                                                                             ifelse("PCT_ELMIDAE" %in% names(metrics.df)[9], "Percentage of Elmidae",
                                                                                                    ifelse("PCT_HYDROPSYCHIDAE" %in% names(metrics.df)[9], "Percentage of Hydropsychidae",
                                                                                                           ifelse("PCT_CHIRONOMIDAE" %in% names(metrics.df)[9], "Percentage of Chironomidae",
                                                                                                                  ifelse("PCT_CORBICULIDAE" %in% names(metrics.df)[9], "Percentage of Corbiculidae", 
                                                                                                                         "Add Metric to ifelse statement"))))))))))))))
    
    
    plot.this(metrics.df, x, plot.title = add.title, yaxis.title = add.yaxis.title, ranges = final.ranges)
  })
  
  png(paste0("output/", x, "_Catch_Curve_", Sys.Date(), ".png"),
      res = 300,
      width = 6.5,
      height = 7,
      units = "in")
  final.plot <- cowplot::plot_grid(plot.list[[2]], plot.list[[1]], plot.list[[3]],
                     align = 'v',
                     ncol = 1)
  
  print(final.plot)
  graphics.off()
  
})
#==============================================================================




