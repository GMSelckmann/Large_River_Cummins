#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 3/10/17
# Updated: 3/10/17
# Maintained: Zachary M. Smith
# Purpose: Keep functions organized and in a designated file for the assessment
#          of Jim Cummins large river data. This will reduce clutter in the
#          anayses scripts.
#==============================================================================
#==============================================================================
# Used in metric_plots.R
# This function iteratively rarefies the sample at multiple sample count levels.
prep_rare <- function(wide.df, reach_year, num.itr){
  # Subset the wide data frame to only include the reach_year of interest.
  reach.df <- wide.df[wide.df$UNIQUE_ID %in% reach_year, ]
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
# Used in metric_plots.R
# This function calculates a standard set of metrics.
large_river_metrics <- function(long.df, master.df){
  metrics.df <- Benthos::ohio_epa_metrics(long.df, master.df)
  #============================================================================
  ord.wide <- wide(long.df, "ORDER")
  fam.wide <- wide(long.df, "FAMILY")
  #============================================================================
  metrics.df$PCT_EPT <- Benthos::pct_ept(ord.wide)
  metrics.df$PCT_PLECOPTERA <- Benthos::pct_taxon(ord.wide, "PLECOPTERA")
  metrics.df$PCT_ELMIDAE <- Benthos::pct_taxon(fam.wide, "ELMIDAE")
  metrics.df$PCT_HYDROPSYCHIDAE <- Benthos::pct_taxon(fam.wide, "HYDROPSYCHIDAE")
  metrics.df$PCT_CHIRONOMIDAE <- Benthos::pct_taxon(fam.wide, "CHIRONOMIDAE")
  metrics.df$PCT_CORBICULIDAE <- Benthos::pct_taxon(fam.wide, "CORBICULIDAE")
  #============================================================================
  return(metrics.df)
}

#==============================================================================
# Used in metric_plots.R
# This function creates the individual metric catch curve plots.
library(ggplot2)
plot.this <- function(metrics.df, metric.col, plot.title, yaxis.title, ranges){
  
  agg.df <- aggregate(metrics.df[, metric.col] ~ SAMPLE_COUNT + STATION_ID,
                      data = metrics.df, function(x) sd(x) / sqrt(length(x)))
  agg.df$MEAN <- aggregate(metrics.df[, metric.col] ~ SAMPLE_COUNT + STATION_ID,
                           data = metrics.df, FUN = mean)[[3]]
  names(agg.df) <- c("SAMPLE_COUNT", "STATION_ID", "SD", "MEAN")
  agg.df$MIN <- agg.df$MEAN - agg.df$SD
  agg.df$MAX <- agg.df$MEAN + agg.df$SD
  limits <- aes(ymax = agg.df$MIN, ymin = agg.df$MAX)
  
  metric.range <- ranges[ranges$METRIC %in% metric.col, ]
  if((metric.range$MAX - metric.range$MIN) >= 0) y.breaks <- seq(0, metric.range$MAX + 1, 1)
  #if((metric.range$MAX - metric.range$MIN) > 5) y.breaks <- seq(0, metric.range$MAX, 5)
  if((metric.range$MAX - metric.range$MIN) > 10) y.breaks <- seq(0, metric.range$MAX + 5, 5)
  if((metric.range$MAX - metric.range$MIN) > 20) y.breaks <- seq(0, metric.range$MAX + 10, 10)
  
  ggplot(agg.df, aes(SAMPLE_COUNT, MEAN, group = STATION_ID, color = STATION_ID)) + 
    labs(#title = plot.title,
      x = "Sample Count",
      y = yaxis.title) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white")) +
    scale_x_continuous(breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
    scale_y_continuous(breaks = y.breaks, limits = c(min(y.breaks), max(y.breaks))) +
    #geom_point() + 
    #stat_smooth(method = "loess") +
    geom_line(size = 1) +
    scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00"),
                       labels = c("2012", "2013", "2014"),
                       #name = "Year") +#,
                       name = plot.title) +#,
    geom_errorbar(limits, width = 30, size = 0.5) #, color = "black")
  #geom_errorbar(width=0.25)
}

#==============================================================================
# Used in metric_plots.R
# This function calculates a standard set of metrics.