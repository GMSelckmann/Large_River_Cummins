#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Organization: ICPRB
# Created: 3/08/17
# Updated: 3/08/17
# Maintained: Zachary M. Smith
# Purpose: Calculate Macroinverterbate metrics for Jim Cummins large river 
#          data set.
# Output: A ".csv" file containing all of the calculated macroinvertebrate
#         metrics.
#==============================================================================
#==============================================================================
# Install and load the Benthos package
#==============================================================================
# Install the package devtools, which makes it easy to install the Benthos
# package.
install.packages(c("devtools","curl", "httr"))
# Install the Benthos package.
devtools::install_github("zsmith27/Benthos", build_vignettes = TRUE, force = TRUE)
# Load the Benthos package.
library(Benthos)
# Load the Master Taxa List contained within the Benthos package.
data(master)
#==============================================================================
# Prepare Jim's data
#==============================================================================
jim.df <- read.csv("data/large_river_cummins_taxa.csv")
# Fill the space between the genus and species name with "_"
jim.df$FINAL_ID <- gsub(" ","_", jim.df$FINAL_ID)

#==============================================================================
id_sub_rank <- function(taxon, taxon.rank, master.df){
  sub.master <- master.df[master.df[, taxon.rank] %in% taxon, ]
  fill.master <- fill_taxa(sub.master)
  final.vec <- unique(fill.master$SPECIES)
  return(final.vec)
}
#==============================================================================
chiro.taxa <- id_sub_rank("CHIRONOMIDAE", "FAMILY", master)
# Roll sub-ranks of Chrinomidae up to Chironomidae.
jim.df$FINAL_ID <- ifelse(jim.df$FINAL_ID %in% chiro.taxa, "CHIRONOMIDAE", jim.df$FINAL_ID)
# Merge the taxonomic counts with the master taxa list.
jim.final <- Benthos::data_prep(jim.df, master)
# Fill in NAs with the previous taxonomic resolution.
jim.final <- Benthos::fill_taxa(jim.final)
#==============================================================================
#==============================================================================
# Make sure any samples with a "D" designation are removed.
jim.final <- jim.final[!grepl("D", jim.final$STATION_ID), ]
# Add some columns to make it easy to aggregate the data.
jim.final$REACH <- substr(jim.final$STATION_ID, 1, 2)
jim.final$DATE <-  as.Date(jim.final$DATE,'%m/%d/%Y')
jim.final$YEAR <- as.numeric(format(jim.final$DATE,'%Y'))
jim.final$REACH_YEAR <- paste(jim.final$REACH, jim.final$YEAR, sep= "_")
#==============================================================================
jim.final$UNIQUE_ID <- jim.final$REACH_YEAR
jim.final$STATION_ID <- 1
jim.final$DATE <- 1
#==============================================================================
# Reorganize the columns to make it easier to perform assessments.
org.cols <- c(1:7, (ncol(jim.final) - 2):ncol(jim.final), 8:(ncol(jim.final) - 3))
jim.final <- jim.final[, org.cols]
#==============================================================================
write.csv(jim.final, "data/reach_year_agg.csv", row.names= FALSE)
#==============================================================================
# Calculate All Metrics
#==============================================================================
# Calculate all of the available metrics.
# You will recieve warning messages letting you know that you did not specify
# a Beck column or a Habit column. This is not an error, it is just letting you
# know that if you were to add these columns to the NYSDEC master taxa list
# you would get a few more metrics.
metrics.df <- Benthos::all_metrics(jim.final, master, "FINAL_ID", 
                          tv.col = "BIBI_TV",
                          ffg.col = "BIBI_FFG",
                          hab.col = "BIBI_HABIT")
#==============================================================================
# Export the table as a ".csv".
#file.name <- paste0("data/large_river_metrics_", Sys.Date(), ".csv")
write.csv(metrics.df, "data/large_river_metrics.csv", row.names = FALSE)

