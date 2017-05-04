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
# Loop through each metric for %Change calculations
#==============================================================================
metric_loop <- function(x, metrics.vec) {
  metric.list <- lapply(metrics.vec, function(metric.x) {
    agg.df <- aggregate(x[, metric.x] ~ SAMPLE_COUNT,
                        data = x, FUN = mean)
    names(agg.df) <- c("SAMPLE_NUMBER", "MEAN")
    agg.df$DIFF_1 <- c(abs(diff(agg.df[, 2])), 0)
    agg.df$PCT_DIFF <- agg.df$DIFF_1 / max(agg.df[, "MEAN"])  * 100
    agg.df$SD <- aggregate(x[, metric.x] ~ SAMPLE_COUNT, data = x, function(x) {
                             sd(x) / sqrt(length(x))
                           })[[2]]
    agg.df$METRIC <- metric.x
    return(agg.df)
  })
  final.df <- do.call(rbind, metric.list)
  # End function metric_loop.
  return(final.df)
}

#==============================================================================
# %Change Calculation
#==============================================================================
pct_change <- function(metrics.df, metrics.vec, station.res) {
  #============================================================================
  if (station.res %in% "ALL") {
    final.df <- metric_loop(metrics.df, metrics.vec)
    final.df$STATION_ID <- "ALL"
  }
  #============================================================================
  if (station.res %in% "STATION") {
    metrics.df$STATION <- substr(metrics.df$STATION_ID, 1, 2)
    station.list <- lapply(unique(metrics.df$STATION), function(station.x) {
      sub.station <- metrics.df[metrics.df$STATION %in% station.x, ]
      final.metric <- metric_loop(sub.station, metrics.vec)
      final.metric$STATION_ID <- station.x
      return(final.metric)
    })
    final.df <- do.call(rbind, station.list)
  }
  #============================================================================
  if (station.res %in% "STATION_YEAR") {
    station.list <- lapply(unique(metrics.df$STATION_ID), function(station.x) {
      sub.station <- metrics.df[metrics.df$STATION_ID %in% station.x, ]
      final.metric <- metric_loop(sub.station, metrics.vec)
      final.metric$STATION_ID <- station.x
      return(final.metric)
    })
    final.df <- do.call(rbind, station.list)
  }
  #============================================================================
  return(final.df)
}
#==============================================================================
# Change names
#==============================================================================

name_change <- function(metrics.df) {
  rich.sub <- function(metrics.df) {
    rich.taxon <- paste(substr(metrics.df$METRIC, 6, nchar(metrics.df$METRIC)))
    rich.taxon <- paste0(substr(rich.taxon, 1, 1),
                         tolower(substr(rich.taxon, 2, nchar(rich.taxon))))
    rich.taxon <- paste(rich.taxon, "Richness")
    return(rich.taxon)
  }
  #---------------------------------------------------------------------------
  pct.sub <- function(metrics.df) {

      pct.taxon <- paste(substr(metrics.df$METRIC, 5, nchar(metrics.df$METRIC)))
      pct.taxon <- paste0(substr(pct.taxon, 1, 1),
                          tolower(substr(pct.taxon, 2, nchar(pct.taxon))))
      pct.taxon <- paste("%", pct.taxon)
      return(pct.taxon)
    
  }
  metrics.df$METRIC <- ifelse(metrics.df$METRIC %in% "RICH", "Richness",
                              ifelse(metrics.df$METRIC %in% "PCT_EPT", "% EPT",
                              ifelse(grepl("RICH_", metrics.df$METRIC), 
                                     rich.sub(metrics.df),
                                     ifelse(grepl("PCT_", metrics.df$METRIC), 
                                            pct.sub(metrics.df),
                                             "ERROR"))))
  return(metrics.df)
}

#==============================================================================
# Metric Class
#==============================================================================
metric_class <- function(metrics.df) {
  rich.metrics <- c("RICH", "RICH_EPHEMEROPTERA", "RICH_TRICHOPTERA", "RICH_DIPTERA")
  metrics.df$CLASS <- ifelse(metrics.df$METRIC %in% rich.metrics, "RICH", "PCT")
  return(metrics.df)
}
#==============================================================================
# Plot %Change
#==============================================================================
plot_pct_change <- function(metrics.df, metrics.vec, metric.type) {
  if (metric.type %in% "rich") {
    sub.metric <- metrics.df[metrics.df$METRIC %in% c("RICH", "RICH_EPHEMEROPTERA",
                                                      "RICH_TRICHOPTERA", "RICH_DIPTERA"), ]
    limits.vec <- c(0, 20)
    breaks.vec <- seq(0, 20, 5)
  }
  
  if (metric.type %in% "pct") {
    sub.metric <- metrics.df[!metrics.df$METRIC %in% c("RICH", "RICH_EPHEMEROPTERA",
                                                       "RICH_TRICHOPTERA", "RICH_DIPTERA",
                                                       "PCT_NON_TANYTARSINI_NON_INSECTA"), ]
    limits.vec <- c(0, 10)
    breaks.vec <- seq(0, 10, 2)
  }
  #----------------------------------------------------------------------------
  # Colorblind palette.
  colorblind <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00",
                  "#0072B2", "#CC79A7", "#999999", "#F0E442",
                  "#000000")
  #----------------------------------------------------------------------------
  sub.metric <- sub.metric[sub.metric$SAMPLE_NUMBER <= 1000, ]
  sub.metric <- name_change(sub.metric)
  ggplot(sub.metric, aes(SAMPLE_NUMBER, PCT_DIFF, group = METRIC,
                         color = METRIC, shape = METRIC)) + 
    labs(#title = plot.title,
      x = "Sample Count",
      y = "Percent Change (%)") +
    #geom_point() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.justification = c(1, 1),
          legend.position = c(1, 1)
          #legend.position="top"
          ) +
    #guides(fill = guide_legend(ncol = 2)) +
    guides(col = guide_legend(ncol = 2)) +
    scale_colour_manual(values = colorblind) +
    #scale_x_continuous(breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
    scale_x_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1100)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 20)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = breaks.vec, expand = c(0, 0), limits = limits.vec) +
    #geom_point() + 
    #stat_smooth(method = "loess") +
    geom_line(size = 1) +
    geom_errorbar(aes(ymax = sub.metric$PCT_DIFF + sub.metric$SD,
                      ymin = sub.metric$PCT_DIFF - sub.metric$SD),
                  width = 30, size = 0.7) +
    geom_hline(aes(yintercept = 2), linetype = "longdash", size = 1) +
    geom_hline(aes(yintercept = 5), linetype = "dotted", size = 1)
  
}



