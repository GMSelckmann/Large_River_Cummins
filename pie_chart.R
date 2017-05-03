
metrics.df <- read.csv("data/large_river_metrics.csv", stringsAsFactors = FALSE)
names(metrics.df)
metrics.df <- metrics.df[, c("UNIQUE_ID", "PCT_GATHER", "PCT_FILTER", "PCT_SHRED", "PCT_SCRAPE", "PCT_PREDATOR")]
agg.df <- tidyr::gather(metrics.df, FFG, PCT, PCT_GATHER:PCT_PREDATOR)

Group <- "FFG"

barplot((agg.df),  #names.arg = levels(param[, (Group)]),
        ylab="Percent (%)", 
        legend.text= TRUE, args.legend = list( bty = "n", cex= 1),
        xlim = c(0,  ncol(agg.df)*2), ylim = c(0, max(colSums(agg.df))+ 5),
        col= brewer.pal(nrow(agg.df), "Paired"), las = 3) 


library(ggplot2)


#' # A pie chart = stacked bar chart + polar coordinates
sub.agg <- agg.df[agg.df$UNIQUE_ID %in% "CR_2012", ]
ggplot(sub.agg, aes(x = factor(1), fill = factor(PCT))) +
  geom_bar(width = 1) + coord_polar(theta = "y")
names(agg.df)

pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1)
pie + coord_polar(theta = "y")

setwd()
for (i in unique(agg.df$UNIQUE_ID)) {
  sub.agg <- agg.df[agg.df$UNIQUE_ID %in% i, ]
  sub.agg <- sub.agg[order(sub.agg$PCT, decreasing = FALSE), ]
  png(paste0(i, ".png"), width = 500, height = 400)
  pie(sub.agg$PCT, labels = sub.agg$FFG, main = unique(sub.agg$UNIQUE_ID))
  dev.off()
}



