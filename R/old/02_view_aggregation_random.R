## script for counting the number of views in analysis, and write total monthly views to rds
library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)

# read in additional functions
source("R/00_functions.R")

# set up vector for languages, classes, and directory for views downloaded from Wikipedia API with Python
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory_new <- "D:/wikipedia_views/user_trends/random_views/random_updated/"

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(languages, directory, view_files){
  
  # bring in all the files in that directory
  view_files <- list.files(directory)
  
  # set up empty list for files for each language
  user_files <- list()
  
  # set up each of the file directories
  for(i in 1:length(languages)){
    user_files[[i]] <- list.files(directory, pattern = languages[i])
    user_files[[i]] <- paste0(directory, "", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files)
}

# run the function with 10 languages, specifying the directory
user_files_new <- view_directories(languages,
                                   directory_new)

# read in all the files in groups for each language
language_views_new <- list()
system.time(for(i in 1:length(user_files_new)){
  language_views_new[[i]] <- lapply(user_files_new[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# remove extra error columns from chinese dataframe - extra dataframe to avoid overwrite
language_views_edit <- language_views_new

# remove extra error columns from dataframes
for(i in 1:length(language_views_edit)){
  for(j in 1:length(language_views_edit[[i]])){
    if("title" %in% colnames(language_views_edit[[i]][[j]])){
      language_views_edit[[i]][[j]] <- language_views_edit[[i]][[j]] %>%
        dplyr::select(-title, -V10) %>%
        dplyr::mutate(views = as.numeric(views))
    }
    
  }
}

# calculate total views for all languages
total_views <- function(data_file){
  view_total <- 0
  for(i in 1:length(data_file)){
    for(j in 1:length(data_file[[i]])){
      view_total <- view_total + sum(data_file[[i]][[j]]$views, na.rm = TRUE)
    }
  }
  print(view_total)
}

# run function for total views
total_views(language_views_edit) # 2227539617 (2.23 billion)

# calculate total views for each language
group_views <- function(data_file){
  language_total <- c(rep(0, 10))
  for(i in 1:length(language_views_edit)){
    for(j in 1:length(language_views_edit[[i]])){
      language_total[[i]] <- language_total[[i]] + sum(language_views_edit[[i]][[j]]$views, na.rm = TRUE)
    }
  }
  
  # build dataframe for views for each language and return it
  language_total <- data.frame("language" = languages, "views" = language_total)
  return(language_total)
}

## script to calculate total monthly views and write to rds
# filter NAs from timestamp
NA_timestamp <- function(data_file){
  data_fin <- data_file %>%
    filter(!is.na(timestamp))
  return(data_fin)
}

# filter NA rows (timestamps) from each set of views 
language_views_monthly <- list()
for(i in 1:length(language_views_edit)){
  language_views_monthly[[i]] <- NA_timestamp(language_views_edit[[i]][[1]])
}

# calculate total monthly views (or daily average views) for each set of views - currently run_dat is set as mean()
average_views_monthly <- list()
for(i in 1:length(language_views_monthly)){
  average_views_monthly[[i]] <- run_dat(language_views_monthly[[i]], av_all = FALSE)
}

# add names for languages and class to each element of the list
names(average_views_monthly) <- languages

# script for saving the updated views
saveRDS(average_views_monthly, "average_daily_views_random_10-languages_updated.rds")
