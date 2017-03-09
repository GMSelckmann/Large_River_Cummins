new.df <- j.df[, names(j.df) %in% c("NAME", "SITE", "YEAR", "STATION_ID", "RICH")]
t.rich <- aggregate(RICH ~ SITE, data = new.df, sum)
names(t.rich) <- c("SITE", "TOTAL_RICH")
merged <- merge(new.df, t.rich, by = "SITE")



#====================================================================
Jim_FC <- Jim
Jim_FC$GENUS <- ifelse(Jim_FC$FAMILY %in% "CHIRONOMIDAE", "CHIRONOMIDAE", as.character(Jim_FC$GENUS))
gen.df <- wide(Jim_FC, "GENUS")
gen.df$DATE <- as.Date(gen.df$DATE, format = "%m/%d/%Y")
gen2 <- gen.df[, 1:5]
gen2 <- gen2[!grepl("D", gen2$STATION_ID),]
gen2$NAME <- substr(gen2$STATION_ID, 1, 2)
gen2$DATE <-  as.Date(gen2$DATE,'%m/%d/%Y')
gen2$YEAR <- as.numeric(format(gen2$DATE,'%Y'))
gen2$STATION <- paste(gsub( "*_.*", "", gen2$STATION_ID), gen2$YEAR, sep = "_")
gen2$SITE <- paste(gen2$NAME, gen2$YEAR, sep= "_")


gen <- merge(gen2, gen.df, by = c("EVENT_ID", "STATION_ID", "DATE", "SAMPLE_NUMBER", "AGENCY_CODE"))

#==============================================================================
gen.site <- gen[, 9:ncol(gen)]
gen.site <- aggregate(. ~ SITE, data = gen.site, sum)
gen.site$RICH_1600 <- vegan::specnumber(gen.site[, 2:ncol(gen.site)])

g.site <- gen.site[, c("SITE", "RICH_1600")]
#==============================================================================
gen.station <- gen[, c(8, 10:ncol(gen))]
#gen.station$STATION <- gsub( "*_.*", "", gen.station$STATION_ID)
gen.station <- aggregate(. ~ STATION, data = gen.station, sum)
gen.station$RICH_400 <- vegan::specnumber(gen.station[, 2:ncol(gen.station)])

g.station <- gen.station[, c("STATION", "RICH_400")]
#=================================================================
gen.reach <- gen[, c(6, 10:ncol(gen))]
#gen.station$STATION <- gsub( "*_.*", "", gen.station$STATION_ID)
gen.reach <- aggregate(. ~ NAME, data = gen.reach, sum)
gen.reach$RICH_4800 <- vegan::specnumber(gen.reach[, 2:ncol(gen.reach)])

g.reach <- gen.reach[, c("NAME", "RICH_4800")]

#==============================================================================
new.df2 <- new.df
new.df2$STATION <- paste(gsub( "*_.*", "", new.df2$STATION_ID), new.df2$YEAR, sep = "_")
merged.1 <- merge(new.df2, g.site, by = "SITE", all.x = T)
merged.2 <- merge(merged.1, g.station,  by = "STATION", all.x = T)
merged <- merge(merged.2, g.reach,  by = "NAME", all.x = T)

merged$COUNT <- ifelse(grepl("100", merged$STATION_ID), 100, 
                       ifelse(grepl("200", merged$STATION_ID), 200, 30000000))
merged$C_400 <- 400
merged$C_1600 <- 1600
merged$C_4800 <- 4800
#=============================================================================
# KX
kx.df <- merged[merged$NAME %in% "KX", ]

plot(kx.df$COUNT, kx.df$RICH)
points(rep(400, nrow(kx.df)), kx.df$RICH_400)

ggplot(kx.df, aes(COUNT, RICH)) + 
  geom_point() +
  geom_point(aes(x = kx.df$C_400, y = kx.df$RICH_400)) +
  geom_point(aes(x = kx.df$C_1600, y = kx.df$RICH_1600)) +
  

catch.curve <- function(data, name, title.me){
  library(ggplot2)
  sub.df <- data[data$NAME %in% name, ]
  sub.100_200 <- sub.df[, c("RICH", "COUNT")]
  sub.400 <- unique(sub.df[, c("STATION", "RICH_400", "C_400")])
  sub.400 <- sub.400[, !names(sub.400) %in% "STATION"]
  names(sub.400) <- c("RICH", "COUNT")
  sub.1600 <- unique(sub.df[, c("SITE", "RICH_1600", "C_1600")])
  sub.1600 <- sub.1600[, !names(sub.1600) %in% "SITE"]
  names(sub.1600) <- c("RICH", "COUNT")
  
  sub.4800 <- unique(sub.df[, c("NAME2", "RICH_4800", "C_4800")])
  sub.4800 <- sub.4800[, !names(sub.4800) %in% "COUNT"]
  names(sub.4800) <- c("NAME2", "RICH")
  sub.4800 <- as.data.frame(sub.4800)
  
  bound <- rbind(sub.100_200, sub.400, sub.1600)
  

  
 ggplot(bound, aes(COUNT, RICH)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ log(x)) + 
    geom_hline(data = sub.4800, aes(yintercept = RICH), color = "black", size = 1) +
   geom_label(data = sub.4800, aes(x = 0, y = RICH, label = "Total Richness"), size = 5, nudge_x = 300) +
    ggtitle(title.me) +
    xlab("Sample Count") +
    ylab("Taxa Richness") +
    ylim(0, 100) +
    theme(text = element_text(size = 25))

}


catch.curve2 <- function(data, name, total.rich, title.me){
  library(ggplot2)
  sub.df <- data[data$NAME %in% name, ]
  sub.100_200 <- sub.df[, c("RICH", "COUNT")]
  sub.400 <- unique(sub.df[, c("STATION", "RICH_400", "C_400")])
  sub.400 <- sub.400[, !names(sub.400) %in% "STATION"]
  names(sub.400) <- c("RICH", "COUNT")
  sub.1600 <- unique(sub.df[, c("SITE", "RICH_1600", "C_1600")])
  sub.1600 <- sub.1600[, !names(sub.1600) %in% "SITE"]
  names(sub.1600) <- c("RICH", "COUNT")
  
  sub.4800 <- unique(sub.df[, c("NAME2", "RICH_4800", "C_4800")])
  #sub.4800 <- sub.4800[, !names(sub.4800) %in% "NAME"]
  names(sub.4800) <- c("NAME2", "RICH", "COUNT")
  sub.4800 <-sub.4800[, !names(sub.4800) %in% "COUNT"]
  sub.4800 <- as.data.frame(sub.4800)
  
  bound <- rbind(sub.100_200, sub.400, sub.1600)

  ggplot(bound, aes(COUNT, RICH)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ log(x)) + 
    geom_hline(data = sub.4800, aes(yintercept = RICH), color = "black", size = 1) +
    geom_label(data = sub.4800, aes(x = 0, y = RICH, label = NAME2), size = 5, nudge_x = 200) +
    geom_hline(data = total.rich, aes(yintercept = RICH), color = "red2", size = 1) +
    geom_label(data = total.rich, aes(x = 0, y = RICH, label = "Total Richness - All 3 Reaches"),
               size = 5, nudge_x = 450) +
    ggtitle(title.me) +
    xlab("Sample Count") +
    ylab("Taxa Richness") +
    ylim(0, 100) +
    theme(text = element_text(size = 25))
  
}

merged2 <- merged[!merged$YEAR %in% 2014, ]


gen.tot <- aggregate(.~ SAMPLE_NUMBER, data = gen[, !names(gen) %in% c("EVENT_ID", "STATION_ID", "DATE",
                                                                       "AGENCY_CODE", "NAME", "YEAR",
                                                                       "STATION", "SITE")], sum)
tot.rich <- data.frame(RICH = ncol(gen.tot) - 1)
tot.rich$COUNT <- 4800

pdf("catch_curves_9_21_16.pdf")
#merged$NAME <- as.character(merged$NAME)
merged$NAME2 <- ifelse(merged$NAME %in% "KX", "Knoxville", 
                      ifelse(merged$NAME %in% "CR", "Carderock", 
                      ifelse(merged$NAME %in% "LF", "Little Falls", "ERROR")))
merged$NAME3 <- ifelse(merged$NAME %in% "KX", "Knoxville Total Richness", 
                       ifelse(merged$NAME %in% "CR", "Carderock Total Richness", 
                              ifelse(merged$NAME %in% "LF", "Little Falls Total Richness", "ERROR")))
catch.curve(merged, "KX", "Knoxville")
catch.curve(merged, "CR", "Carderock")
catch.curve(merged, "LF", "Little Falls")
catch.curve2(merged, c("KX", "CR", "LF"), tot.rich,  "Potomac River All 3 Reaches")

dev.off()
