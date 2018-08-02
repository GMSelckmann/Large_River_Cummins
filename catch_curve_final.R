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
#setwd("C:/Users/zsmith/Desktop/Large_River/Jim_Cummins/Large_River_Cummins")
source("metric_calc.R")
source("pct_change.R")
source("catch_curve_plot.R")
#==============================================================================
# Create a dataframe from the metrics.list output.
metrics.df <- do.call(rbind, metrics.list)
write.csv(metrics.df, paste0("metrics_", Sys.Date(), ".csv"), row.names = FALSE)
#==============================================================================
#metrics.df <- read.csv("C:/Users/zsmith/Desktop/Large_River/Jim_Cummins/Large_River_Cummins/metrics_5_4_17.csv", stringsAsFactors = FALSE)
#==============================================================================
#metrics.df <- metrics.df[!metrics.df$STATION_ID %in% "LF_2013", ]
#metrics.df <- metrics.df[metrics.df$SAMPLE_COUNT  <= 1200, ]
metrics.vec <- names(metrics.df[, 9:ncol(metrics.df)])
#==============================================================================
# %change based on all values.
#all.df <- pct_change(metrics.df, metrics.vec, "ALL")
# %change based on each stations values.
#station.df <- pct_change(metrics.df, metrics.vec, "STATION")
# %change based on each station and years values.
station_year.df <- pct_change(metrics.df, metrics.vec, "STATION_YEAR")
station_year.df$STATION <- substr(station_year.df$STATION_ID, 1, 2)
#==============================================================================
#==============================================================================
kx.plot <- catch_curve_plot(station_year.df[station_year.df$STATION %in% "kx", ])
cr.plot <- catch_curve_plot(station_year.df[station_year.df$STATION %in% "cr", ])
lf.plot <- catch_curve_plot(station_year.df[station_year.df$STATION %in% "lf", ])
#==============================================================================
# setwd("C:/Users/zsmith/Desktop/Large_River/Jim_Cummins/Large_River_Cummins/output/6_15_18")
"C:/Users/zsmith/Desktop/Large_River/Jim_Cummins/Large_River_Cummins/output/6_15_18/catch_curve.png" %>% 
png(width = 5.5, height = 7.5, units = "in", res = 1200)
grid.newpage()
grid.draw(rbind(ggplotGrob(kx.plot), ggplotGrob(cr.plot), ggplotGrob(lf.plot), size = "last"))
dev.off()


