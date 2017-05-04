#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 5/03/2017
# Updated: 5/03/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
library(ggplot2)
library(grid)
#==============================================================================
source("metric_calc.R")
source("pct_change.R")
#==============================================================================
# Create a dataframe from the metrics.list output.
metrics.df <- do.call(rbind, metrics.list)
metrics.df <- metrics.df[!metrics.df$STATION_ID %in% "LF_2013", ]
metrics.df <- metrics.df[metrics.df$SAMPLE_COUNT  <= 1200, ]
#==============================================================================
# %change based on all values.
all.df <- pct_change(metrics.df, metrics.vec, "ALL")
# %change based on each stations values.
station.df <- pct_change(metrics.df, metrics.vec, "STATION")
# %change based on each station and years values.
station_year.df <- pct_change(metrics.df, metrics.vec, "STATION_YEAR")
#==============================================================================
#==============================================================================
rich.plot <- plot_pct_change(all.df, metrics.vec, "rich")
pct.plot <- plot_pct_change(all.df, metrics.vec, "pct")
grid.newpage()
grid.draw(rbind(ggplotGrob(rich.plot), ggplotGrob(pct.plot), size = "last"))
#==============================================================================
