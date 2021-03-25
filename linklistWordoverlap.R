########### Creating a link list for TM model based on word overlap  ###########
#------------------------------------------------------------------------------#
### This script describes the journey from the diagnostics file which is       #
### automatically given out from MALLET to a link list for a term overlap network. 

### No extra packages are needed.                
### Variable topic_am in depends on the path. 

### Code was written for different pairs of models, of which one was considered the "main" model, 
### the other one the "control" model. 

### Directory must include the diagnostics file output from MALLET. 
### Diagnostics files (MALLET output) were labelled "diagnostics_Xt_100w.csv" 
### for the main model and "d_Xt_100w.csv" for control models. 
### 

#------------------------------------------------------------------------------#
# Load paths
#------------------------------------------------------------------------------#
paths<- c("~\\Topic Modeling Results\\10t_100w\\bigram\\control",
          "~\\Topic Modeling Results\\10t_100w\\bigram",
          "~\\Topic Modeling Results\\10t_100w\\nobigram\\control",
          "~\\Topic Modeling Results\\10t_100w\\nobigram",
          "~\\Topic Modeling Results\\50t_100w\\bigram\\control",
          "~\\Topic Modeling Results\\50t_100w\\bigram",
          "~\\Topic Modeling Results\\50t_100w\\nobigram\\control",
          "~\\Topic Modeling Results\\50t_100w\\nobigram",
          "~\\Topic Modeling Results\\200t_100w\\nobigram",
          "~\\Topic Modeling Results\\100t_100w\\bigram", 
          "~\\Topic Modeling Results\\500t_100w\\bigram"
)

for (p in 1: length(paths)){
  #p <- 1
  wd <- paths[[p]]
  #------------------------------------------------------------------------------#
  # Determine directory & model: control or main?
  #------------------------------------------------------------------------------#  
  if(unlist(strsplit(wd, "\\\\"))[length(unlist(strsplit(wd, "\\\\")))] == "control"){
    model <- "control"
  }else{
    model <- "main"
  }
  if(unlist(strsplit(wd, "\\\\"))[length(unlist(strsplit(wd, "\\\\")))] == "control2"){
    model <- "control"
  } 
  topic_am <- as.numeric(strsplit(unlist(strsplit(wd, "\\\\"))[7], "t_")[[1]][1]) #ATTENTION! depends on the path
  
  
  #------------------------------------------------------------------------------#
  # Load the diagnostics file: 
  # Watch the path! 
  #------------------------------------------------------------------------------#
  
  if(model == "main"){
    diagnostics <- read.csv2(paste0(wd, "\\diagnostics_", topic_am, "t_100w.csv"))
  }else{
    diagnostics <- read.csv2(paste0(wd, "\\d_", topic_am, "t_100w_c.csv"))
  }
  
  
  #------------------------------------------------------------------------------#
  # Create a dataframe with all possible combinations between the topic numbers
  #------------------------------------------------------------------------------#
  combinations <- as.data.frame(combn(unique(diagnostics$id), 2))
  
  
  #------------------------------------------------------------------------------#
  # The loop shows how many words are intersected and creates a list of the words 
  # that overlap
  #------------------------------------------------------------------------------#
  
  lengths_intersect <- vector()
  words_intersect <- vector("list", length = ncol(combinations))
  
  for( i in 1: ncol(combinations)){
    # i <- 50
    lengths_intersect[i] <- length(intersect(diagnostics$word[which(diagnostics$id == combinations[1,i])], 
                                             diagnostics$word[which(diagnostics$id ==combinations[2,i])]))
    words_intersect [[i]] <- intersect(diagnostics$word[which(diagnostics$id == combinations[1,i])], 
                                       diagnostics$word[which(diagnostics$id ==combinations[2,i])])
  }
  
  
  #------------------------------------------------------------------------------#
  # Here the lists can be examined  
  #------------------------------------------------------------------------------#
  # 
  # mean(lengths_intersect)
  # summary(lengths_intersect)
  # 
  # no <- 30
  #   
  # combinations[,which(lengths_intersect >= no)]
  # words_intersect[which(lengths_intersect >= no)]
  # 
  # 
  # #
  # combinations[,which(combinations[1,] == 31 )]
  # 
  # combinations$V865
  # lengths_intersect[865]
  # 
  #------------------------------------------------------------------------------#
  # First, we turn the combinations table around and subsequently add the lengths 
  # to the combinations. We rename the columns to "source", "target" and "value 
  # and save the linklist to the correct folder
  #------------------------------------------------------------------------------#
  
  combinations_linklist <- as.data.frame(t(combinations))
  combinations_linklist$value <- lengths_intersect
  colnames(combinations_linklist) <- c("source", "target", "value")
  path_network <- paste0(wd, "\\networks")
  
  if(!dir.exists(path_network)){
    dir.create(path_network)
  }
  
  if(model == "main"){
    write.csv2(combinations_linklist, paste0(path_network, "\\", "linklist_", topic_am, "t_100w.csv"),row.names = FALSE )
  }else{
    write.csv(combinations_linklist, paste0(path_network, "\\", "linklist_", topic_am, "t_100w_wordsoverlap_control.csv"),row.names = FALSE )
  }
  
  print(paste("fertig mit path ", paths[[p]]))
  
}
################################################################################
################################################################################