

library(RColorBrewer)

stacked <- function(all_taxa.df, FFG, Group, Attribute.Group,
                    Taxa.List = Taxon_List, Info = Taxa_Info, agg.fun = mean){
  colnames(all_taxa.df) <- gsub("^PCT_?", "", colnames(all_taxa.df))
  ffg <- split(Info$FAMILY, Info[, Attribute.Group])
  filter <- ffg[[FFG]]
  group.df <- all_taxa.df[, colnames(all_taxa.df) %in% filter]
  fam.df<- group.df[, colnames(group.df) %in% Taxa.List$FAMILY]
  fg.df <- cbind(all_taxa.df[, 1:5], fam.df)
  t_name <- fg.df[, 6:ncol(fg.df)]
  param <- cbind(fg.df[, Group], t_name)
  colnames(param)[1] <- c(Group)
  agg_t <- aggregate(param[,2:ncol(param)], list(param[, Group]), FUN = agg.fun)
  agg_t <- t(agg_t[2:ncol(agg_t)])
  windows(width=12, height= 10)
  par( mar=c(2,2,2,2), oma=c(5,1,1,1))
  barplot((agg_t),  names.arg = levels(param[, (Group)]),
          ylab="Percent (%)", 
          legend.text= TRUE, args.legend = list( bty = "n", cex= 1),
          xlim = c(0,  ncol(agg_t)*2), ylim = c(0, max(colSums(agg_t))+ 5),
          col= brewer.pal(nrow(agg_t), "Paired"), las = 3) 
  title(FFG, adj=0.36, cex.main=2)
}

Wide.df <- fam_merge
FFG <- "CG"
Group <- "NAME"
Attribute.Group <-"BIBI_FFG"
Info <- master2
insert.title <- "Scraper"

stacked2 <- function(Wide.df, FFG, Group, Attribute.Group,
                      Info = master2, insert.title = FFG){
  
  prep.data <- Wide.df[, -which(names(Wide.df) %in% c("DATE", "REPLICATE", "AGENCY_CODE"))]
  agg_fam <- aggregate(prep.data[,7:ncol(prep.data)], list(prep.data[, Group]), FUN = mean)
  agg_fam[, 2:ncol(agg_fam)] <- (agg_fam[, 2:ncol(agg_fam)] / rowSums(agg_fam[, 2:ncol(agg_fam)])) *100
  colnames(agg_fam)[1] <- c(Group)
  
  ffg <- split(Info$FAMILY, Info[, Attribute.Group])
  filter <- ffg[FFG]
  group.df <- agg_fam[, colnames(agg_fam) %in% unlist(filter)]
  
  fg.df <- cbind(agg_fam[, Group], group.df)
  t_name <- fg.df[, 2:ncol(fg.df)]
  param <- cbind(fg.df[, 1], t_name)
  colnames(param)[1] <- c(Group)
  
  
  agg_t <- t(param[2:ncol(param)])
  colnames(agg_t) <- param[, Group]
  agg_t <- agg_t[order(-apply(agg_t, 1, sum)), ]
  
  tol21rainbow= c("#771155", "#AA4488", "#CC99BB",
                  "#114477", "#4477AA", "#77AADD",
                  "#117777", "#44AAAA", "#77CCCC",
                  "#117744", "#44AA77", "#88CCAA",
                  "#777711", "#AAAA44", "#DDDD77",
                  "#774411", "#AA7744", "#DDAA77",
                  "#771122", "#AA4455", "#DD7788")
  roundUp <- function(x) 10^ceiling(log10(x))
  
  #windows(width=12, height= 10)
  #par( mar=c(2,2,2,2), oma=c(5,1,1,1))
  barplot((agg_t),  names.arg = colnames(agg_t),
          ylab = "Percent (%)", 
          #space = c(0, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2),
          legend.text= TRUE, args.legend = list(x=ncol(agg_t) + 3.5,
                                                #y=roundUp(max(rowSums(t(agg_t))))/1.25,
                                                y = ceiling(max(rowSums(t(agg_t)))),
                                                bty = "n", cex= 1),
          xlim = c(0,  ncol(agg_t) * 2.5), ylim = c(0, if(ceiling(max(rowSums(t(agg_t)))) <= 20){
            ceiling((max(rowSums(t(agg_t)))) / 2) * 2
          }else{
            if(ceiling(max(rowSums(t(agg_t)))) <= 50){
              ceiling((max(rowSums(t(agg_t)))) / 5) * 5
            }else{
              if(ceiling(max(rowSums(t(agg_t)))) <= 80){
              ceiling((max(rowSums(t(agg_t)))) / 10) * 10
              }else{
                100
            }
            
          }}),
                                                    #(round(max(rowSums(t(agg_t)))) / 5) *5),
          #col= unlist(brewer.pal(12, "Paired"), "gray0"), las = 3)
          col = if(nrow(agg_t) > 12){
            tol21rainbow
            }else{
              brewer.pal(12, "Paired")
            })
  title(insert.title, adj=0.36, cex.main=2)
}

#fam_merge<- merge(J_INFO, FAM, by = c("EVENT_ID", "STATION_ID"))
#gen_test<- merge(J_INFO, GEN, by = c("EVENT_ID", "STATION_ID"))

fam <- wide(Jim, "FAMILY")
fam_merge <- merge(j.df[, 1:4], fam, by = "STATION_ID")


pdf("POT_LGR_STACKED_9_9_16.pdf")
#FFGs
stacked2(fam_merge, "SC", Group = "NAME", "BIBI_FFG", insert.title = "Scraper")
stacked2(fam_merge, "PR", Group = "NAME", "BIBI_FFG", insert.title = "Predator")
stacked2(fam_merge, "CG", Group = "NAME", "BIBI_FFG", insert.title = "Gather")
stacked2(fam_merge, "CF", Group = "NAME", "BIBI_FFG", insert.title = "Filter Feeder")
stacked2(fam_merge, c("CF", "CG"), Group = "NAME", "BIBI_FFG", insert.title = "Collector")

#Habits
stacked2(fam_merge, "BU", Group = "NAME", "BIBI_HABIT", insert.title = "Burrower")
stacked2(fam_merge, "CB", Group = "NAME", "BIBI_HABIT", insert.title = "Climber")
stacked2(fam_merge, "CN", Group = "NAME", "BIBI_HABIT", insert.title = "Clinger")
stacked2(fam_merge, "SP", Group = "NAME", "BIBI_HABIT", insert.title = "Sprawler")
stacked2(fam_merge, "SW", Group = "NAME", "BIBI_HABIT", insert.title = "Swimmer")
#Urban Intolerant
stacked2(fam_merge, "1", Group = "NAME", "INTOLERANT_URBAN", insert.title = "Urban Intolerant")

dev.off()

stacked2(fam_merge, c("SP", "SP, BU", "SP, CN", "SP, SK",
                      "CB, SP", "CB, SP, CN", "CB,SP",
                      "CB, SP, CN", "CN, SP", "CN, SP, BU"),
         Group = "SITE", "HABITAT", insert.title = "SPRAWLER")

stacked2(fam_merge, c("BU", "BU, CB", "BU, CN",
                        "SP, BU", "CN, SP, BU"),
         Group = "SITE", "HABITAT", insert.title = "Burrower")

stacked2(fam_merge, c("SW", "SW, CN", "CB, SW",
                      "CN, SW", "CN, SP, SW"),
         Group = "SITE", "HABITAT", insert.title = "Swimmer")

#==============================================================================
#YEAR
stacked2(fam_merge, "PR", Group = "YEAR", "GUILD", insert.title = "Predator")
stacked2(fam_merge, "SC", Group = "YEAR", "GUILD", insert.title = "Scraper")
stacked2(fam_merge, c("BU", "BU, CB", "BU, CN",
                      "SP, BU", "CN, SP, BU"),
         Group = "YEAR", "HABITAT", insert.title = "Burrower")


#==============================================================================
#NAME
stacked2(fam_merge, c("SP", "SP, BU", "SP, CN", "SP, SK",
                      "CB, SP", "CB, SP, CN", "CB,SP",
                      "CB, SP, CN", "CN, SP", "CN, SP, BU"),
         Group = "NAME", "HABITAT", insert.title = "SPRAWLER")

stacked2(fam_merge, c("CF", "CG"), Group = "NAME", "GUILD", 
         insert.title = "Collector")


stacked2(fam_merge, "SC", Group = "NAME", "GUILD", insert.title = "Scraper")

stacked2(fam_merge, c("BU", "BU, CB", "BU, CN",
                      "SP, BU", "CN, SP, BU"),
         Group = "NAME", "HABITAT", insert.title = "Burrower")

stacked2(fam_merge, c("CN", "CN, CB", "BU, CN",
                      "CB, SP, CN", "CN, CB, SP",
                      "CN, SP", "CN, SP, BU",
                      "CN, SP, SW", "CN, SW",
                      "SP, CN", "SW, CN"),
         Group = "NAME", "HABITAT", insert.title = "Clinger")


stacked2(fam_merge, "X", Group = "NAME", "INTOLERANT_URBAN", 
         insert.title = "Urban Intolerant")


stacked2(fam_merge, c("SW", "SW, CN", "CB, SW",
                      "CN, SW", "CN, SP, SW"),
         Group = "NAME", "HABITAT", insert.title = "Swimmer")








stacked2(fam_merge, "CG", Group = "NAME", "GUILD")
stacked(all_taxa, "PR", Group = "STATION_ID", "GUILD")
stacked(all_taxa, "CF", Group = "STATION_ID", "GUILD")
stacked(all_taxa, "SC", Group = "STATION_ID", "GUILD")
stacked(all_taxa, "SH", Group = "STATION_ID", "GUILD")

stacked(all_taxa, "X", Group = "NAME", "NON_INSECT")
stacked(all_taxa, "I", Group = "STATION_ID", "Tolarance_class")

all_taxa2 <- all_taxa
all_taxa2[grepl("_200C", all_taxa2$STATION_ID), 6:ncol(all_taxa2)]<- all_taxa2[grepl("_200C", all_taxa2$STATION_ID), 6:ncol(all_taxa2)] * 2

test <- aggregate(all_taxa2[, c(6:ncol(all_taxa2))], list(all_taxa2$BANK), FUN = sum)
test[,2:ncol(test)] <- test[,2:ncol(test)]/4

test2 <- test[, c("Group.1", "PCT_BAETIDAE", "PCT_CORBICULIDAE")]
test <- FAM[c(1,4,7),]
tt <- rowSums(test[,6:ncol(test)])
sum(tt)


BANK <-  gsub("_.*", "", J_INFO$STATION_ID)
test$BANK <- paste(BANK, test$YEAR, sep ="_")
test$BANKS <- 



colnames(all_taxa) <- gsub("^PCT_?", "", colnames(all_taxa))
ffg <- split(Taxa_Info$FAMILY, Taxa_Info$GUILD)
filter <- ffg[c("CF", "CG")]
group.df <- all_taxa[, colnames(all_taxa) %in% unlist(filter)]
fam.df<- group.df[, colnames(group.df) %in% Taxon_List$FAMILY]
fg.df <- cbind(all_taxa[, 1:4], fam.df)
t_name <- fg.df[, 5:ncol(fg.df)]
test <- cbind(fg.df[, "NAME"], t_name)
colnames(test)[1] <- c("NAME")
agg_t <- aggregate(test[,2:ncol(test)], list(test[ ,"NAME"]),
                   FUN = function(x){
                     ts <- sum(x)
                   })
ff <- function(x){
  ts <- sum(x[,2:ncol(x)] )
  tt <- ts[,2:ncol(ts)] / rowSums(ts[,2:ncol(ts)]) * 100
  return(tt)
}

ttt <- ff(agg_t)

test5 <- agg_t[,2:ncol(agg_t)]/rowSums(agg_t[,2:ncol(agg_t)]) *100
tt <- rowSums(test5)
agg_t <- t(agg_t[2:ncol(agg_t)])
windows(width=12, height= 10)
barplot((agg_t),  names.arg = unique(test[, "NAME"]),
        ylab="Percent (%)", 
        legend.text= TRUE, args.legend = list(x = 5.5, y = 30, bty = "n", cex= 1),
        xlim=c(0, ncol(agg_t) +3), ylim=c(0, (max(colSums(agg_t)))+5),
        col= seq(0, 500, by= 5)) 
title("CF", adj=0.36, cex.main=2)