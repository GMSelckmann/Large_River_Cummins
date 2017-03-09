setwd("C:\\Users\\Owner\\Desktop\\Test_12_4_15\\Test")

library(BIBI2)
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
FAM$NAME <- substr(FAM$STATION_ID, 1, 2)
FAM$DATE <-  as.Date(FAM$DATE,'%m/%d/%Y')
FAM$YEAR <- as.numeric(format(FAM$DATE,'%Y'))
FAM$SITE <- paste(FAM$NAME, FAM$YEAR, sep= "_")
FAM$COLOR[FAM$YEAR == 2012]="purple"
FAM$COLOR[FAM$YEAR == 2013]="blue"
FAM$COLOR[FAM$YEAR == 2014]="red"
FAM$COLOR[FAM$SITE %in% "CR_2012"]="green"
FAM$COLOR[FAM$YEAR %in% "CR_2013"]="yellow"
FAM$COLOR[FAM$YEAR %in% "CR_2014"]="red"
NEW_FAM <- subset(FAM, sel = c(EVENT_ID:AGENCY_CODE, NAME:COLOR, ANCYLIDAE:UNIONIDAE))

test<-vegan::vegdist(FT)
test2 <-MASS::isoMDS(test)
test3 <- metaMDS(FT, k =3)
stems <- colSums(FT) 
windows()
plot(test3, dis = "sp", type = "n", xlim = c(-1,1), ylim = c(-0.5, 0.6)) 
#sel <- orditorp(test3, dis = "sp",  priority = stems,  pcol = "gray", pch = "+")
points(test3, display = "sites", cex = 0.8, pch=22, col=NEW_FAM$COLOR, bg=NEW_FAM$COLOR)
ordihull(test3, NEW_FAM$SITE, display="sites", col="black", label=TRUE)
text(test3, display = "spec", cex=0.7, col="blue")
text(test3, display = "sites", cex=0.7, col="blaCK")
#ord.fit<-envfit(ord~Distance.from.Shore+Temperature+Conductivity+pH, data=Reduced_ENV, na.rm=TRUE,col=1, bg=1)
#plot(ord.fit, col="black", cex=1)

