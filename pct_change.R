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

metric_loop <- function(x, metrics.vec) {
  metric.list <- lapply(metrics.vec, function(metric.x) {
    agg.df <- aggregate(x[, metric.x] ~ SAMPLE_COUNT,
                        data = x, FUN = mean)
    names(agg.df) <- c("SAMPLE_NUMBER", "MEAN")
    agg.df$DIFF_1 <- c(abs(diff(agg.df[, 2])), 0)
    agg.df$MAX <- c(agg.df$MEAN[-1], 0)
    agg.df$DIFF <- ifelse(agg.df$MAX == 0, 0 , agg.df$DIFF_1 / agg.df$MAX * 100)
    
    #agg.df$PCT_DIFF <- agg.df$DIFF / max(agg.df[, 2])  * 100
    agg.df$PCT_DIFF <- agg.df$DIFF_1 / max(agg.df[, 2])  * 100
    #agg.df$DIFF <- c(max(agg.df[, 2]), 0) * 100
    agg.df$SD <- aggregate(x[, metric.x] ~ SAMPLE_COUNT, data = x, function(x) {
                             sd(x) / sqrt(length(x))
                           })[[2]]
    agg.df$METRIC <- metric.x
    #agg.df$STATION_ID <- station.x
    return(agg.df)
  })
  final.df <- do.call(rbind, metric.list)
  # End function metric_loop.
  return(final.df)
}

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






