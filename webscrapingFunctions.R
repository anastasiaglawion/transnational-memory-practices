#### Functions for AHF web scraping  ###########################################
# Directory: "scraping_final" contains following data: 
#               |_userinfo.csv
#               |_louvain_clusters.csv
#               |_forum_ids.txt

# A valid username and password are needed for the content extraction.

#### Set WD ####################################################################
setwd("~\\scraping_final")

#### Package list ##############################################################

#install.packages(c("xlsx","httr","pbapply", "stringr", "tm", "plyr", "dplyr", "utils", "data.table", "countrycode", "igraph")) 

library(rvest)
library(xlsx)
library(httr)
library(pbapply)
library(stringr)
library(tm)
library(plyr)
library(dplyr)
library(utils)
library(data.table)
library(countrycode)
library(igraph)
library(foreach)
library(doSNOW)
library(magrittr)
library(RCurl)

userinfo <- read.csv("userinfo.csv", sep = ",", stringsAsFactors = F)[,-1]

#### Logging in ################################################################

url       <-"https://forum.axishistory.com/memberlist.php"
session <- html_session(url)
form    <-html_form(session)[[2]]
filled_form <- set_values(form,
                          "username" = "##ENTER_VALID_USERNAME", 
                          "password" = "##ENTER_VALID_PASSWORD")
submit_form(session,filled_form)


# Check if logged in (watch Size:) # 
jump_to(session, url)

################################################################################
#### MAIN PAGE #################################################################
main_page <- "http://forum.axishistory.com/index.php"

#### 1.1 Number of threads per subforum on the first page ######################
first_page_threads <- function(url){  
  page <- jump_to(session, url)
  topic_am <- html_nodes(x = read_html(page), css = ".row .topics") 
  as.numeric(removeWords(html_text(topic_am, trim = TRUE), " Topics"))
}

#### 1.1 Number of posts per subforum

posts <- function(url){  
  page <- jump_to(session, url)
  post_am <- html_nodes(x = read_html(page), css = ".row .posts") 
  as.numeric(removeWords(html_text(post_am, trim = TRUE), " Posts"))
}

# returns a numerical vector

#### 2. Urls for each subforum #################################################

forum_url <- function(url){  
  page <- jump_to(session, url)
  forum_title <- html_nodes(x = read_html(page), css = ".forumtitle") 
  df <- bind_rows(lapply(xml_attrs(forum_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  return(df$href)
}

#### 3. Forum Titles ###########################################################
forum_title <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page), css = ".forumtitle"), trim =T)
}

################################################################################
#### Level 1: Subforums ########################################################
forum_ids <- as.numeric(scan("Forum_ids.txt", what = "numeric"))[1:54]

#### 4. Number of pages in each subforum #######################################
page_numbers <- function(url){  
  page <- jump_to(session, url)
  html_text(html_node(x = read_html(page) , css = ".top .ellipsis+ li a"), 
            trim = TRUE)
}

#### 5. Topic Titles ###########################################################
# Function for Topic Titles:
topic_title <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page), css = ".first a"), trim = TRUE)
}

getFirstPost <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page), css = ".content"), trim = TRUE)[[1]]
}



#### 6. Topic URLs #############################################################
topic_url <- function(url){
  page <- jump_to(session, url)
  extr_topic_title <- html_nodes(x = read_html(page), css = ".topictitle")
  topic_links <- bind_rows(lapply(xml_attrs(extr_topic_title), function(x) data.frame(as.list(x), 
                                                                                      stringsAsFactors=FALSE)))
  return(topic_links$href)
}

################################################################################
#### Level 2: Threads ##########################################################
#### 7. Top in Threads #########################################################
#top <- function(url){  
#  page <- jump_to(session, url)
#  html_text(html_nodes(x = read_html(page), css = ".bar-top .pagination") , trim = TRUE)
#}
#### 7.1 Thread length in Threads ##############################################
thread_length <- function(url){  
  page <- jump_to(session, url)
  as.numeric(sapply(strsplit(html_text(html_nodes(x = read_html(page), css = ".bar-top .pagination") , trim = T), " "), "[[",1))
}


#### 8. Post Content ###########################################################

post_content <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".content") , trim = TRUE)
}


#### 8.1. Post title  ##########################################################

post_title <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".topic-title a") , trim = TRUE)
}


#### 8.2 Date of first post  ###################################################

first_post_date <- function(url){  
  page <- jump_to(session, url)
  sapply(strsplit(html_text(html_nodes(x = read_html(page) , css = ".bar-top+ .bg2 .post-number-bold+ .author") , trim = TRUE), "» "), "[[", 2)
}


#### 8. Usernames from post ####################################################

usernames <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".responsive-hide .username") , trim = TRUE)
}

#### 8.1 Usernames from post - 2 ###############################################

usernames2 <- function(url){  
  page <- jump_to(session, url)
  unique(html_text(html_nodes(x = read_html(page) , css = ".responsive-hide .username") , trim = TRUE))
}

#### 9. User-ID from post ######################################################

user_ids_from_post <- function(url){
  page <- jump_to(session, url)
  extr_user_id <- html_nodes(x = read_html(page), css = ".responsive-hide .username")
  ids <- bind_rows(lapply(xml_attrs(extr_user_id), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  as.numeric(substr(ids$href, 37, 50))
}

#### 10. Post metadata #########################################################

post_meta <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".post-number-bold+ .author") , trim = TRUE)
}


#### 10.1. Post Id #############################################################
post_ids <- function(url){
  page <- jump_to(session, url)
  post_id <- html_nodes(x = read_html(page), css = ".post-number-bold a")
  ids <- bind_rows(lapply(xml_attrs(post_id), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  as.numeric( sapply(strsplit(ids$href, "#p"), "[[", 2))
}

#### 10.2 Post Date  ###########################################################


post_date_time <- function(url){  
  page <- jump_to(session, url)
  sapply(strsplit(html_text(html_nodes(x = read_html(page) , css = ".post-number-bold+ .author") , trim = TRUE), " » "), "[[", 2)
}

#### 10.3 Post No. Within Thread  ##############################################


post_no_within_thread <- function(url){
  page <- jump_to(session, url)
  as.numeric(sapply(strsplit(html_text(html_nodes(x = read_html(page) , css = ".post-number-bold") , trim = TRUE), "#"), "[[",2))
}

#### 11. Status ################################################################

status <- function(url){
  x <- jump_to(session, url)
  return(x)
}


#### 12. Memberlist ############################################################

usernames_memberlist <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".bg2 .posts , .bg1 .posts , #memberlist .username") , trim = TRUE)
}


user_ids_memberlist <- function(url){
  page <- jump_to(session, url)
  extr_user_id <- html_nodes(x = read_html(page), css = "#memberlist .username")
  ids <- bind_rows(lapply(xml_attrs(extr_user_id), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  substr(ids$href, 37, 50)
}

rank_memberlist <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = "td .rank-img") , trim = TRUE)
}




#### 13. Profile information: upper segment (Name, Age, Location, etc.) ########
prof_info <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".profile-details dd , .profile-details dt") , trim = TRUE)
}


#### Profile information: lower right segment (Joined, Last active)
prof_info_joined <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".column2 dd:nth-child(4) , .column2 dd:nth-child(2)") , trim = TRUE)
}

prof_info_total_posts <- function(url){  
  page <- jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".column2 dd:nth-child(6)") , trim = TRUE)
}

#### 15. Topic Title from post #################################################
topic_title_post <- function(url2){  
  page <- jump_to(session, url2)
  html_text(html_nodes(x = read_html(page), css = ".topic-title a") , trim = TRUE)
}



#### 16. combining matrices  ###################################################

complete_matrix <- function(x, y) {
  dif <- setdiff(rownames(y), rownames(x))
  mat0 <- matrix(0, length(dif), ncol(x), dimnames = list(dif, NULL))
  colnames(mat0) <- colnames(x)
  mat1 <- rbind(x, mat0)
  mat2 <- cbind(mat1, matrix(0, nrow(mat1), length(dif), dimnames = list(NULL, dif)))
  return(mat2)
}



#### 16.1 combining dataframes 

complete_df <- function(x, y) {
  dif <- setdiff(rownames(y), rownames(x))
  df0 <- data.frame(0, length(dif), ncol(x), dimnames = list(dif, NULL))
  colnames(df0) <- colnames(x)
  mat1 <- rbind(x, mat0)
  mat2 <- cbind(mat1, matrix(0, nrow(mat1), length(dif), dimnames = list(NULL, dif)))
  return(mat2)
}




#### For some reason url.exists does not work with AHF
## Source: https://stackoverflow.com/a/60627969
####
valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

################################################################################
# Data on Louvain clustering ###################################################
louvain <- read.csv("louvain_clusters.csv", 
                    sep= ";")
colnames(louvain) <- c("cluster_sqrt", "userID")

