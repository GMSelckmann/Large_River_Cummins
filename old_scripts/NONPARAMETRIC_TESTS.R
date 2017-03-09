dunn_tb <- function(data.df, Group){
  
  nt <- lapply(data.df[, 5:ncol(data.df)], function(x) dunn.test::dunn.test(x, data.df[, Group], kw = TRUE, label = TRUE))
  df <- data.frame(names(nt), matrix(unlist(nt),
                                     ncol = 1 + 4 * length(unique(nt$RICH$comparisons)),
                                     byrow = T), stringsAsFactors = FALSE)
  colnames(df) <- c("Metric", "Dunn_Chi2", paste("Z", nt$RICH$comparisons, sep="_"), paste("Dunn_P_Value", nt$RICH$comparisons, sep="_") , paste("P_Adjust", nt$RICH$comparisons, sep="_"))
  grepped <- grep("Dunn_P_Value", colnames(df))
  dunn_pvalue <- df[,c(1, grepped)]
  if(length(grepped) > 1){
    dunn_pvalue[, 2:ncol(dunn_pvalue)] <- apply(dunn_pvalue[, 2:ncol(dunn_pvalue)], 2,
                                                function(x) as.numeric(as.character(x)))
  }else{
    if(z < 0 ){
      next
    }else{
      dunn_pvalue[, 2:ncol(dunn_pvalue)] <- as.numeric(dunn_pvalue[, 2:ncol(dunn_pvalue)])
    
    }
  }
  
  
  dunn_pvalue[,2:ncol(dunn_pvalue)] <- round(dunn_pvalue[,2:ncol(dunn_pvalue)], digits = 4)
  return(dunn_pvalue)
}

kruskal_tb <- function(data.df, Group){
  
  nt <- lapply(data.df[, 5:ncol(data.df)], function(x) kruskal.test(x ~ data.df[, Group]))
  df <- data.frame(matrix(unlist(nt), nrow =  length(nt), byrow=T),stringsAsFactors=FALSE)
  colnames(df) <- c("kw_Statistic", "df", "kw_p_value", "Test", "Description")
  
  new.df <- df[, 1:3]
  new.df$kw_Statistic <- round(as.numeric(new.df$kw_Statistic), digits = 4)
  new.df$kw_p_value <- round(as.numeric(new.df$kw_p_value), digits = 4)
  
  metrics.df <- data.frame(colnames(data.df[, 5:ncol(data.df)]))
  colnames(metrics.df) <- "Metric"
  cbound <- cbind(metrics.df, new.df)
  return(cbound)
}

kd_tb <- function(data.df, Group){
  dunn.df <- dunn_tb(data.df, Group)
  kruskal.df <- kruskal_tb(data.df, Group)
  both.df <- merge(kruskal.df, dunn.df, by = "Metric")
  return(both.df)
}




wilcox_tb <- function(data.df, Group){
  
  nt <- lapply(data.df[, 5:ncol(data.df)], function(x) wilcox.test(x ~ data.df[, Group]))
  df <- data.frame(matrix(unlist(nt), nrow =  length(nt), byrow=T), stringsAsFactors=FALSE)
  
  new.df <- data.frame(df[, 1:2])
  colnames(new.df) <- c("Wilcox_Statistic", "Wilcox_p_value")
  new.df$Wilcox_Statistic <- round(as.numeric(new.df$Wilcox_Statistic), digits = 4)
  new.df$Wilcox_p_value <- round(as.numeric(new.df$Wilcox_p_value), digits = 4)
  
  metrics.df <- data.frame(colnames(data.df[, 5:ncol(data.df)]))
  colnames(metrics.df) <- "Metric"
  cbound <- cbind(metrics.df, new.df)
  return(cbound)
}