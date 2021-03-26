# ------------------------------------------------------------------------------
# Visualize Cluster Metadata: location (Chapter 3)
# ------------------------------------------------------------------------------
# Prepare the environment: remove all objects and activate the functions 
# ------------------------------------------------------------------------------
rm(list = ls())
source("~\\webscrapingFunctions.R")
library(RColorBrewer)


# ------------------------------------------------------------------------------
# Working directories for reading data and writing data: 
# ------------------------------------------------------------------------------
files <-  list.files(pattern = "*_filtered.csv")
wd_write <- "~\\Visualizations\\Chapter 3"

# ------------------------------------------------------------------------------
# Format cluster data
# ------------------------------------------------------------------------------
cluster_data <- as.data.frame(table(louvain$cluster_sqrt))
interesting_clusters <- cluster_data$Var1[1:10]
usergroups <- vector("list", length = length(interesting_clusters))

for(i in 1:length(interesting_clusters)){

    usergroups[[i]] <- louvain$userID[which(louvain$cluster == interesting_clusters[i])]

    }

names(usergroups) <- interesting_clusters
# ------------------------------------------------------------------------------
# Find out most represented countries and create colors for them
# ------------------------------------------------------------------------------

setwd(wd_read)
top50 <- list()

for ( cl in 1: length(usergroups)){
  
  #cl <- 1
  
  u <- usergroups[[cl]] 

  subset_userinfo <- subset(userinfo, userID %in% u)
  
  loc_table <- as.data.frame(table(subset_userinfo$loc_code))
  
  loc_table$Var1 <- as.character(loc_table$Var1)
  
  loc_table$Freq <- as.numeric(loc_table$Freq)
  
  loc_table <- loc_table[order(loc_table$Freq, decreasing = T),]
  
  top50[[cl]] <- loc_table$Var1[1:50]

  }


top50_allclusters <-as.character(unique(unlist( top50)))
top50_allclusters <- top50_allclusters[-which(is.na(top50_allclusters))]


n <- length(top50_allclusters)


qual_col_pals = brewer.pal.info[brewer.pal.info$category == "qual",]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colors <- data.frame(top50_allclusters, sample(col_vector, n))

colnames(colors) <- c("country", "color")


# ------------------------------------------------------------------------------
# Create the visualizations per cluster
# ------------------------------------------------------------------------------

for ( cl in 1: length(usergroups)){
  #cl <- 2
  u <- usergroups[[cl]] 
  subset_userinfo <- subset(userinfo, userID %in% u)
  loc_table <- as.data.frame(table(subset_userinfo$loc_code))
  loc_table <- loc_table[order(loc_table$Freq, decreasing = T),]

  for( i in 1: length (loc_table$Var1)){
    if(length(colors$color[which(loc_table$Var1[i] == colors$country)]) !=0){
      loc_table$color[i] <- as.character(colors$color[which(loc_table$Var1[i] == colors$country)])
      }else{
      loc_table$color[i] <- "#000000"
      }
  }
    
    png(paste0(wd_write, "\\pie_loc_cl", names(usergroups)[[cl]], ".png"), width= 800, height = 800)
    par(mar = c(0.5,.5,.5,.5)) # Essential for small margins
    
    pie(loc_table$Freq, labels = loc_table$Var1[1:20], 
        #main=paste("Countries in cluster", names(usergroups)[[cl]]), 
        col=loc_table$color,
        cex = 2
        )
    
    dev.off() 
    
  #View(loc_table)
  
  
}

# ------------------------------------------------------------------------------
# Create the visualizations for AHF 
# ------------------------------------------------------------------------------

loc_table_all <- as.data.frame(table(userinfo$loc_code))
loc_table_all <- loc_table_all[order(loc_table_all$Freq, decreasing = T),]
for( i in 1: length (loc_table_all$Var1)){
  if(length(colors$color[which(loc_table_all$Var1[i] == colors$country)]) !=0){
    loc_table_all$color[i] <- as.character(colors$color[which(loc_table_all$Var1[i] == colors$country)])
  }else{
    loc_table_all$color[i] <- "#000000"
  }
}


png(paste0(wd_write, "\\pie_loc_all.png"), width= 800, height = 800)
par(mar = c(0.5,.5,.5,.5))

pie(loc_table_all$Freq, 
    labels = loc_table_all$Var1[1:20], 
    #main=paste("Countries on AHF in general"), 
    col=loc_table_all$color, 
    cex = 2)
dev.off() 
