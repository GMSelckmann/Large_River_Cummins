

catch_curve_plot <- function(metrics.df) {
  sub.metric <- metrics.df[metrics.df$METRIC %in% "RICH", ]
  sub.metric$YEAR <- substr(sub.metric$STATION_ID, 4, 7)
  #----------------------------------------------------------------------------
  # Colorblind palette.
  colorblind <- c("#56B4E9", "#009E73", "#E69F00")
  #----------------------------------------------------------------------------
  leg.title <- ifelse(metrics.df$STATION %in% "KX", "Knoxville",
                      ifelse(metrics.df$STATION %in% "CR", "Carderock",
                             ifelse(metrics.df$STATION %in% "LF", "Little Falls", "ERROR")))
  #----------------------------------------------------------------------------
  #sub.metric <- sub.metric[sub.metric$SAMPLE_NUMBER <= 1000, ]
  sub.metric <- name_change(sub.metric)
  ggplot(sub.metric, aes(SAMPLE_NUMBER, MEAN, group = YEAR,
                         color = YEAR)) + 
    labs(#title = plot.title,
      x = "Sample Count",
      y = "Richness") +
    #geom_point() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"),
          #legend.title = leg.title,
          legend.text = element_text(size = 8),
          legend.justification = c(1, 1),
          legend.position = c(1, 1)
          #legend.position="top"
    ) +
    #guides(fill = guide_legend(ncol = 2)) +
    labs(color = leg.title) +
    scale_colour_manual(values = colorblind) +
    #scale_x_continuous(breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
    scale_x_continuous(breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 20)) +
    #scale_y_continuous(breaks = y.breaks, limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 60, 10), expand = c(0, 0), limits = c(0, 60)) +
    #geom_point() + 
    #stat_smooth(method = "loess") +
    geom_line(size = 0.7) +
    geom_errorbar(aes(ymax = sub.metric$MEAN + sub.metric$SD,
                      ymin = sub.metric$MEAN - sub.metric$SD),
                  width = 30, size = 0.7)
  
}
