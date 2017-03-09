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
# Install the package devtools, which makes it easy to install the Benthos
# package.
install.packages(c("devtools","curl", "httr"))
require("devtools")
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
prep_rare <- function(wide.df, reach_year, num.itr){
  # Subset the wide data frame to only include the reach_year of interest.
  reach.df <- wide.df[wide.df$UNIQUE_ID %in% "CR_2012", ]
  #============================================================================
  # Identify what the nearest 100 value is.
  # if the value was rounded up (total < rounded value), then subtract 100.
  total <- sum(reach.df[, 8:ncol(wide.df)])
  if (total < round(total, -2)) {
    end.sub <- round(total, -2) - 100
  } else {
    end.sub <- round(total, -2)
  }
  # Using intervals of 100, generate a sequence from 100 to the specified end.sub.
  sub.samp.seq <- seq(100, end.sub, 100)
  #============================================================================
  # Loop through each sub.samp.seq and use the sub.samp.seq value as the sample
  # size for the rarefaction function.
  loop.rare <- lapply(sub.samp.seq, function(x){
    print(x)
    # For sub.samp.seq equal to x, iterate the rarefaction sampling process for
    # the specified number of interations (num.itr).
    rare.list <- lapply(1:num.itr, function(y){
      print(paste("[", x, "] ", y, "/", num.itr))
      sub.samp <- vegan::rrarefy(reach.df[, 8:ncol(wide.df)], x)
    })
    # Bind the list output from rare.list.
    final.df <- data.frame(do.call(rbind, rare.list))
    # Generate the unique ID by pasting x followed by the number of specified 
    # iterations. Example if x = 1600 and  num.itr = 100:
    # First UNIQUE_ID = 1600.1
    # Last UNIQUE_ID = 1600.100
    final.df$UNIQUE_ID <- paste(x, seq(1, num.itr, 1), sep = ".")
    # Re-arrange the columns.
    final.df <- final.df[, c(ncol(final.df), 1:(ncol(final.df) - 1))]
  })
  # Row bind the looped rarefaction lists to create a data frame.
  bound <- do.call(rbind, loop.rare)
  #============================================================================
  # Transform the data format from long to wide.
  long.df <- tidyr::gather(bound, FINAL_ID, REPORTING_VALUE, 2:ncol(bound))
  # Remove all of the taxa counts equal to 0.
  long.df <- long.df[long.df$REPORTING_VALUE > 0, ]
  # Make the reach_year the station_id.
  long.df$STATION_ID <- reach_year
  # Use this function to quickly/easily generate and fill many of the columns
  # necessary for the Benthos package.
  long.df <- Benthos::benthos_cheat(long.df)
  # Prep the data.
  prep.df <- Benthos::data_prep(long.df, Benthos::master)
  # Fill in NAs with the lowest taxonomic resolution.
  final.df <- Benthos::fill_taxa(prep.df)
  #============================================================================
  return(final.df)
}
#==============================================================================
# Carderock Samples
rare.CR_2012 <- prep_rare(wide.df, "CR_2012", 10)
rare.CR_2013 <- prep_rare(wide.df, "CR_2013", 10)
rare.CR_2014 <- prep_rare(wide.df, "CR_2014", 10)
# Knoxville Samples
rare.KX_2012 <- prep_rare(wide.df, "KX_2012", 10)
rare.KX_2013 <- prep_rare(wide.df, "KX_2013", 10)
rare.KX_2014 <- prep_rare(wide.df, "KX_2014", 10)
# Little Falls Samples
rare.LF_2012 <- prep_rare(wide.df, "LF_2012", 10)
rare.LF_2013 <- prep_rare(wide.df, "LF_2013", 10)
rare.LF_2014 <- prep_rare(wide.df, "LF_2014", 10)
# Make list of all the data frames
data.list <- list(rare.CR_2012, rare.CR_2013, rare.CR_2014,
                  rare.KX_2012, rare.KX_2013, rare.KX_2014,
                  rare.LF_2012, rare.LF_2013, rare.LF_2014)
#==============================================================================
# Calculate Metrics
#==============================================================================
# Calculate all of the available metrics.
# You will recieve warning messages letting you know that you did not specify
# a Beck column or a Habit column. This is not an error, it is just letting you
# know that if you were to add these columns to the NYSDEC master taxa list
# you would get a few more metrics.

# Use this script to run all metrics for an individual reach.
metrics.df <- Benthos::all_metrics(rare.CR_2012, master, "GENUS", 
                                   tv.col = "BIBI_TV",
                                   ffg.col = "BIBI_FFG",
                                   hab.col = "BIBI_HABIT")
metrics.df$SAMPLE_COUNT <- as.numeric(gsub("\\..*","", metrics.df$UNIQUE_ID))

# Use this script to run all metrics for all reaches.
all.site.metrics <- lapply(data.list, function(x){
  metrics.df <- Benthos::all_metrics(rare.df, master, "GENUS", 
                                     tv.col = "BIBI_TV",
                                     ffg.col = "BIBI_FFG",
                                     hab.col = "BIBI_HABIT")
  metrics.df$SAMPLE_COUNT <- as.numeric(gsub("\\..*","", metrics.df$UNIQUE_ID))
  return(metrics.df)
})
#==============================================================================
# Calculate Richness

# Use this script to calulate richness for one river reach.
wide.rare <- wide(rare.CR_2012, "GENUS")
metrics.df <- wide.rare[, 1:7]
metrics.df$RICH <- vegan::specnumber(wide.rare[, 8:ncol(wide.rare)])
metrics.df$SAMPLE_COUNT <- as.numeric(gsub("\\..*","", metrics.df$UNIQUE_ID))

# Use this script to calculate richness for all reaches.
all.site.rich <- lapply(data.list, function(x){
  wide.rare <- wide(x, "GENUS")
  metrics.df <- wide.rare[, 1:7]
  metrics.df$RICH <- vegan::specnumber(wide.rare[, 8:ncol(wide.rare)])
  metrics.df$SAMPLE_COUNT <- as.numeric(gsub("\\..*","", metrics.df$UNIQUE_ID))
  return(metrics.df)
})

#==============================================================================
# Function for plotting catch curves.
library(ggplot2)
plot.this <- function(metrics.df, metric.col){
  agg.df <- aggregate(metrics.df[, metric.col] ~ SAMPLE_COUNT, data = metrics.df, FUN = sd)
  agg.df$MEAN <- aggregate(metrics.df[, metric.col] ~ SAMPLE_COUNT, data = metrics.df, FUN = mean)[[2]]
  names(agg.df) <- c("SAMPLE_COUNT", "SD", "MEAN")
  agg.df$MIN <- agg.df$MEAN - agg.df$SD
  agg.df$MAX <- agg.df$MEAN + agg.df$SD
  limits <- aes(ymax = agg.df$MIN, ymin = agg.df$MAX)
  
  ggplot(agg.df, aes(SAMPLE_COUNT, MEAN)) + 
    labs(title = unique(metrics.df$STATION_ID),
         x = "Sample Count",
         y = metric.col) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_continuous(breaks = seq(0, 1600, 100), limits = c(0, 1600)) +
    #geom_point() + 
    #stat_smooth(method = "loess") +
    geom_line() +
    geom_errorbar(limits, width = 30)
    #geom_errorbar(width=0.25)
}


plot.this(metrics.df, "RICH")
#==============================================================================
# Use this to generate Richness plots for each river reach.
lapply(all.site.rich, function(x){
  plot.this(x, "RICH")
})

# Use this to geneerate Richness plots for one river reach.
plot.this(metrics.df, "RICH")
plot.this(metrics.df, "PCT_ELMIDAE")
plot.this(metrics.df, "PCT_EPT")
plot.this(metrics.df, "PCT_EPHEMEROPTERA")
plot.this(metrics.df, "PCT_PLECOPTERA")
plot.this(metrics.df, "PCT_TRICHOPTERA")
plot.this(metrics.df, "PCT_CHIRONOMIDAE")
#==============================================================================

aggregate(RICH ~ SAMPLE_COUNT, data = metrics.df, function(x) quantile(x, 0.5))
aggregate(RICH ~ SAMPLE_COUNT, data = metrics.df, FUN = mean)
quantile(metrics.df$RICH, 0.5)



