setwd("C:\\Users\\Owner\\Desktop\\Test_12_4_15\\Test")

library(BIBI)
master
library(dunn.test)
library(vegan)
library(MASS)
#==============================================================================
Taxa_Info <- read.csv("dbo_TAB_FAMILY_Update_12_22_15.csv")
Taxon_List <- read.csv("TaxonList_ntb_01-07-2016_2.csv", header = TRUE)
New_List <- data.frame(fill_taxa(Taxon_List))

jim_data <- read.csv("JIM_C_DATA_3.csv")
Jim <- merge(jim_data, New_List, by.x = "TAXON_NAME", by.y = "FINAL_ID", all.x = TRUE)
colnames(Jim) <- toupper(colnames(Jim))
jim.df <- Jim[, c("EVENT_ID", "STATION_ID", "DATE", "REPLICATE", "AGENCY_CODE",
                  "PHYLUM", "SUBPHYLUM", "CLASS", "SUBCLASS", "ORDER",
                  "SUBORDER", "FAMILY", "SUBFAMILY", "TRIBE", "GENUS", "TSN_R",
                  "REPORTING_VALUE")]
colnames(jim.df) <- c("EVENT_ID", "STATION_ID", "DATE", "REPLICATE", "AGENCY_CODE",
                      "PHYLUM", "SUBPHYLUM", "CLASS", "SUBCLASS", "ORDER",
                      "SUBORDER", "FAMILY", "SUBFAMILY", "TRIBE", "GENUS", "TSN",
                      "REPORTING_VALUE")
FAM <- wide(jim.df, "FAMILY")
ORD <- wide(jim.df, "ORDER")
GEN <- wide(jim.df, "GENUS")

FAM$NAME <- substr(FAM$STATION_ID, 1, 2)
FAM$DATE <-  as.Date(FAM$DATE,'%m/%d/%Y')
FAM$YEAR <- as.numeric(format(FAM$DATE,'%Y'))
FAM$SITE <- paste(FAM$NAME, FAM$YEAR, sep= "_")
Metrics <- all_metrics(Taxa_Info, jim.df)

#merged <- merge(unique(jim_data[,c("EVENT_ID", "DATE")]), Metrics, by = "EVENT_ID")


fj <- Metrics[!grepl("D", Metrics$STATION_ID),]
fj$NAME <- substr(fj$STATION_ID, 1, 2)
fj$DATE <-  as.Date(fj$DATE,'%m/%d/%Y')
fj$YEAR <- as.numeric(format(fj$DATE,'%Y'))

fj$SITE <- paste(fj$NAME, fj$YEAR, sep= "_")

j.df <- subset(fj, select=c(NAME, SITE, YEAR, STATION_ID,RICH:PCT_LIMESTONE))
j.df <- j.df[ , -which(names(j.df) %in% c("PCT_SKATE"))]
j.df$NAME <- as.factor(j.df$NAME)
j.df$SITE <- as.factor(j.df$SITE)

median(j.df[j.df$NAME %in% "KX", "SHANNON"])

j.df$SITE <- factor(j.df$SITE, c("KX_2012", "KX_2013", "KX_2014",
                                 "CR_2012", "CR_2013", "CR_2014",
                                 "LF_2012", "LF_2013", "LF_2014"))
j.df$NAME <- factor(j.df$NAME, c("KX", "CR", "LF"))

j.df$STATION_ID <- factor(j.df$STATION_ID, c("KXvlUL_100A", "KXvlUL_100B", "KXvlUL_200C",
                                             "KXvlUR_100A", "KXvlUR_100B", "KXvlUR_200C",
                                             "KXvlLL_100A", "KXvlLL_100B", "KXvlLL_200C",
                                             "KXvlLR_100A", "KXvlLR_100B", "KXvlLR_200C",
                                             "CRckUL_100A", "CRckUL_100B", "CRckUL_200C",
                                             "CRckUR_100A", "CRckUR_100B", "CRckUR_200C",
                                             "CRckLL_100A", "CRckLL_100B", "CRckLL_200C",
                                             "CRckLR_100A", "CRckLR_100B", "CRckLR_200C",
                                             "LFUL_100A", "LFUL_100B", "LFUL_200C",
                                             "LFUR_100A", "LFUR_100B", "LFUR_200C",
                                             "LFLL_100A", "LFLL_100B", "LFLL_200C",
                                             "LFLR_100A", "LFLR_100B", "LFLR_200C"))


nonpar_site <- kd_tb(j.df, "SITE")
write.csv(nonpar_site, "nonpar_site2.csv")
nonpar_name <- kd_tb(j.df, "NAME")
write.csv(nonpar_name, "nonpar_name2.csv")
nonpar_year <- kd_tb(j.df, "YEAR")
write.csv(nonpar_year, "nonpar_year2.csv")
nonpar_station <- kd_tb(j.df, "STATION_ID")
write.csv(nonpar_station, "nonpar_station.csv")

sig_metrics <- function(nonpar.df, sig_level){
  sig_metrics <- nonpar.df[nonpar.df$kw_p_value <= sig_level, ]
  ordered_sig_metrics <- sig_metrics[order(sig_metrics$kw_p_value), ]
  final.df <- ordered_sig_metrics$Metric
  return(final.df)
}

                                          
#==============================================================================
pdf(file="JIM_SITES2.pdf", width = 6, height = 6)
windows()
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(j.df)){
  a <- colnames(j.df[i])
  c_list <- list('KX_12', 'KX_13', 'KX_14',
                   'CR_12', 'CR_13', 'CR_14',
                   'LF_12', 'LF_13', 'LF_14')
  boxplot(j.df[, i] ~ SITE,
          data = j.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}

#==============================================================================
sig_site_metrics.df <- j.df[, names(j.df) %in% sig_metrics(nonpar_site, 0.01)]
sig_site.df <- cbind(j.df[,1:4], sig_site_metrics.df)
site_list <- c("PCT_HYDRO_EPT", "PCT_SCRAPE", "PCT_PREDATOR", "ASPT_MOD",
               "PCT_EPT", "BECKS", "PCT_TRICHOPTERA", "PCT_URBAN_INTOL",
               "PCT_COLLECT", "PCT_SIMULIIDAE", "PCT_SPRAWL", "FBI", "PCT_PLECOPTERA")
sig_site.df <- cbind(j.df[, 1:4], j.df[, names(j.df) %in% site_list])
pdf(file="JIM_SIG_SITES2.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(sig_site.df)){
  a <- colnames(sig_site.df[i])
  c_list <- list('KX_12', 'KX_13', 'KX_14',
                 'CR_12', 'CR_13', 'CR_14',
                 'LF_12', 'LF_13', 'LF_14')
  boxplot(sig_site.df[, i] ~ SITE,
          data = sig_site.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}
#==============================================================================
pdf(file="JIM_YEAR2.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(j.df)){
  a <- colnames(j.df[i])
  c_list <- list('2012', '2013', '2014')
  boxplot(j.df[, i] ~ YEAR,
          data = j.df,
          names = c('2012', '2013', '2014'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

#==============================================================================
sig_year_metrics.df <- j.df[, names(j.df) %in% sig_metrics(nonpar_year, 0.01)]
sig_year.df <- cbind(j.df[,1:4], sig_year_metrics.df)
sig_year.df <- cbind(j.df[, 1:4], j.df[, names(j.df) %in% c("PCT_PREDATOR", "PCT_SIMULIIDAE", "PCT_EPT", "PCT_HYDRO_EPT")])
pdf(file="JIM_SIG_YEAR2.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(sig_year.df)){
  a <- colnames(sig_year.df[i])
  c_list <- list('2012', '2013', '2014')
  boxplot(sig_year.df[, i] ~ YEAR,
          data = sig_year.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}
#========================================================================================
pdf(file="JIM_NAME2.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(j.df)){
  a <- colnames(j.df[i])
  c_list <- list('KX', 'CR', 'LF')
  boxplot(j.df[, i] ~ NAME,
          data = j.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}

#==============================================================================
sig_name_metrics.df <- j.df[, names(j.df) %in% sig_metrics(nonpar_name, 0.01)]
sig_name.df <- cbind(j.df[,1:4], sig_name_metrics.df)

site_list <- c("ASPT_MOD", "PCT_SPRAWL", "PCT_HYDRO_EPT",
               "BECKS", "PCT_COLLECT", "PCT_CLING", "PCT_EPT",
               "PCT_SIMULIIDAE", "PCT_PLECOPTERA", "PCT_EPT_TAXA_RICH")
sig_name.df <- cbind(j.df[, 1:4], j.df[, names(j.df) %in% site_list])
pdf(file="JIM_SIG_NAME2.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(sig_name.df)){
  a <- colnames(sig_name.df[i])
  c_list <- list('KX', 'CR', 'LF')
  boxplot(sig_name.df[, i] ~ NAME,
          data = sig_name.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}
#==============================================================================

pdf(file="JIM_STATION.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(6,4,4,2), oma=c(2,0,0,0))
#***
for (i in 5:ncol(j.df)){
  a <- colnames(j.df[i])
  c_list <- list("KXvlUL_100A", "KXvlUL_100B", "KXvlUL_200C",
                   "KXvlUR_100A", "KXvlUR_100B", "KXvlUR_200C",
                   "KXvlLL_100A", "KXvlLL_100B", "KXvlLL_200C",
                   "KXvlLR_100A", "KXvlLR_100B", "KXvlLR_200C",
                   "CRckUL_100A", "CRckUL_100B", "CRckUL_200C",
                   "CRckUR_100A", "CRckUR_100B", "CRckUR_200C",
                   "CRckLL_100A", "CRckLL_100B", "CRckLL_200C",
                   "CRckLR_100A", "CRckLR_100B", "CRckLR_200C",
                   "LFUL_100A", "LFUL_100B", "LFUL_200C",
                   "LFUR_100A", "LFUR_100B", "LFUR_200C",
                   "LFLL_100A", "LFLL_100B", "LFLL_200C",
                   "LFLR_100A", "LFLR_100B", "LFLR_200C")
  boxplot(j.df[, i] ~ STATION_ID,
          data = j.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}
#==============================================================================
sig_station_metrics.df <- j.df[, names(j.df) %in% sig_metrics(nonpar_station, 0.05)]
sig_station.df <- cbind(j.df[,1:4], sig_station_metrics.df)

pdf(file="JIM_SIG_STATION.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(6,4,4,2), oma=c(2,0,0,0))
#***
for (i in 5:ncol(sig_station.df)){
  a <- colnames(sig_station.df[i])
  c_list <- list("KXvlUL_100A", "KXvlUL_100B", "KXvlUL_200C",
                 "KXvlUR_100A", "KXvlUR_100B", "KXvlUR_200C",
                 "KXvlLL_100A", "KXvlLL_100B", "KXvlLL_200C",
                 "KXvlLR_100A", "KXvlLR_100B", "KXvlLR_200C",
                 "CRckUL_100A", "CRckUL_100B", "CRckUL_200C",
                 "CRckUR_100A", "CRckUR_100B", "CRckUR_200C",
                 "CRckLL_100A", "CRckLL_100B", "CRckLL_200C",
                 "CRckLR_100A", "CRckLR_100B", "CRckLR_200C",
                 "LFUL_100A", "LFUL_100B", "LFUL_200C",
                 "LFUR_100A", "LFUR_100B", "LFUR_200C",
                 "LFLL_100A", "LFLL_100B", "LFLL_200C",
                 "LFLR_100A", "LFLR_100B", "LFLR_200C")
  boxplot(sig_station.df[, i] ~ STATION_ID,
          data = sig_station.df,
          names = c_list,
          col="white", las = 2,
          main = substitute(paste(a)))
}


#

















split_all <- split(j.df, j.df$NAME)
cr.df <- split_all$CR
kx.df <- split_all$KX
lf.df <- split_all$LF

cr_year <- kd_tb(cr.df, "SITE")

pdf(file="JIM_CR.pdf", width = 6, height = 6)
windows()
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))

#***
for (i in 5:ncol(cr.df)){
  a <- colnames(cr.df[i])
  c_list <- list('CR_12', 'CR_13','CR_14')
  boxplot(cr.df[, i] ~ YEAR,
          data = cr.df,
          names = c('CR_12', 'CR_13','CR_14'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

pdf(file="JIM_KX.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(kx.df)){
  a <- colnames(kx.df[i])
  c_list <- list('KX_12', 'KX_13','KX_14')
  boxplot(kx.df[, i] ~ YEAR,
          data = kx.df,
          names = c('KX_12', 'KX_13','KX_14'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

pdf(file="JIM_LF.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(lf.df)){
  a <- colnames(lf.df[i])
  c_list <- list('LF_12', 'LF_13','LF_14')
  boxplot(lf.df[, i] ~ YEAR,
          data = lf.df,
          names = c('LF_12', 'LF_13','LF_14'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

#==============================================================================
split_year <- split(j.df, j.df$YEAR)
y12.df <- split_year$`2012`
y13.df <- split_year$`2013`
y14.df <- split_year$`2014`

pdf(file="JIM_12.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(y12.df)){
  a <- colnames(y12.df[i])
  c_list <- list('CR', 'KX', 'LF')
  boxplot(y12.df[, i] ~ NAME,
          data = y12.df,
          names = c('CR', 'KX', 'LF'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

pdf(file="JIM_13.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(y13.df)){
  a <- colnames(y13.df[i])
  c_list <- list('CR', 'KX', 'LF')
  boxplot(y13.df[, i] ~ NAME,
          data = y13.df,
          names = c('CR', 'KX', 'LF'),
          col="white", las = 2,
          main = substitute(paste(a)))
}

pdf(file="JIM_14.pdf", width = 6, height = 6)
par(mfrow=c(1,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
#***
for (i in 5:ncol(y14.df)){
  a <- colnames(y14.df[i])
  c_list <- list('CR', 'KX', 'LF')
  boxplot(y14.df[, i] ~ NAME,
          data = y14.df,
          names = c('CR', 'KX', 'LF'),
          col="white", las = 2,
          main = substitute(paste(a)))
}
