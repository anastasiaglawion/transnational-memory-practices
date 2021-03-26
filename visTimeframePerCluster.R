# ------------------------------------------------------------------------------
# Visualize Cluster Metadata: Timeframe (Chapter 3)
# ------------------------------------------------------------------------------
# Directory must contain the tables with posts and usernames per cluster. 
# ------------------------------------------------------------------------------
# Prepare the environment: remove all objects and activate the functions 
# ------------------------------------------------------------------------------
rm(list = ls())
source("~\\webscrapingFunctions.R")

#-------------------------------------------------------------------------------
# Working directories for reading data and writing data: 
#-------------------------------------------------------------------------------
wd_read <- "~\\clustertexts\\csv_allcl"
setwd(wd_read)
wd_write <- "~\\Visualizations\\Chapter 3\\"
files <-  list.files(pattern = "*_filtered.csv") 

# ------------------------------------------------------------------------------
# Import cluster data
# ------------------------------------------------------------------------------
cluster_data <- as.data.frame(table(louvain$cluster_sqrt))
interesting_clusters <- as.character(cluster_data$Var1)

# ----------------------------------------------------------------------------------------------------
# Examine metadata: years
# ----------------------------------------------------------------------------------------------------
years <- 2002:2018

for(cl in 1: length(interesting_clusters)){
  #cl <- 1
  setwd(wd_read)
  posts <- read.csv(paste0("cluster", interesting_clusters[[cl]],"_filtered.csv"), stringsAsFactors = F)[,-1]
  table <- as.data.frame(table(posts$year))
  table$Var1 <-  as.numeric(as.character(table$Var1))
  table <- table[which(table$Var1 %in% years),]
  png(paste0(wd_write, "rplot_cluster",interesting_clusters[cl], ".png"), width= 800, height = 600)
  x <- barplot(height = table$Freq, 
               names.arg = table$Var1,
               xlab = "Year",
               ylab = "Number of posts",
               #xlim = c(0,18),
               #main = paste("Amount of posts per year in cluster", interesting_clusters[[cl]]),
               col = "salmon", xaxt = "n", cex.axis = 2
  )
  
  axis(1,at = x, labels = table$Var1, cex.axis=2, tick = T)
  #text(x, y = 20, table$Freq, cex=1, pos = 3)
  dev.off()
}


