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


plot_pct_change <- function(metrics.df, metrics.vec, metric.type) {
  if (metric.type %in% "rich") {
    sub.metric <- metrics.df[metrics.df$METRIC %in% c("RICH", "RICH_EPHEMEROPTERA",
                                              "RICH_TRICHOPTERA", "RICH_DIPTERA"), ]
  }
  
  if (metric.type %in% "pct") {
    sub.metric <- metrics.df[!metrics.df$METRIC %in% c("RICH", "RICH_EPHEMEROPTERA",
                                               "RICH_TRICHOPTERA", "RICH_DIPTERA"), ]
  }
  
  sub.metric <- sub.metric[sub.metric$SAMPLE_NUMBER <= 1000, ]
  ggplot(sub.metric, aes(SAMPLE_NUMBER, PCT_DIFF, group = METRIC, color = METRIC)) + 
    labs(#title = plot.title,
      x = "Sample Count",
      y = "Percent Change (%)") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white")) +
    #scale_x_continuous(breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
    scale_x_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1100)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 20)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 10, 2), expand = c(0, 0), limits = c(0, 10)) +
    #geom_point() + 
    #stat_smooth(method = "loess") +
    geom_line(size = 1) +
    geom_errorbar(aes(ymax = sub.metric$PCT_DIFF + sub.metric$SD,
                      ymin = sub.metric$PCT_DIFF - sub.metric$SD),
                  width = 30, size = 0.7) +
    geom_hline(aes(yintercept = 2), linetype = "longdash", size = 1)
  
}

plot_pct_change(all.df, metrics.vec, "pct")
plot_pct_change(all.df, metrics.vec, "rich")
#==============================================================================
names(agg.df) <- c("SAMPLE_COUNT", "MEAN", "DIFF", "SD")
agg.df$MIN <- agg.df$DIFF - agg.df$SD
agg.df$MAX <- agg.df$DIFF + agg.df$SD
limits <- aes(ymax = agg.df$MIN, ymin = agg.df$MAX)

metric.range <- ranges[ranges$METRIC %in% metric.col, ]
if((metric.range$MAX - metric.range$MIN) >= 0) y.breaks <- seq(0, metric.range$MAX + 1, 1)
#if((metric.range$MAX - metric.range$MIN) > 5) y.breaks <- seq(0, metric.range$MAX, 5)
if((metric.range$MAX - metric.range$MIN) > 10) y.breaks <- seq(0, metric.range$MAX + 5, 5)
if((metric.range$MAX - metric.range$MIN) > 20) y.breaks <- seq(0, metric.range$MAX + 10, 10)