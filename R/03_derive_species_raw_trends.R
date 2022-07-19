#!/usr/bin/env Rscript

## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# set extra library path fro when running from Python
.libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )

# read in packages, note that located across two different folders
library(dplyr)
library(parallel)
library(data.table)

# set up cores for parallel processing
cl <- makeCluster(detectCores())

# set working directory for base corr
working_dir <- "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/"

# read in the rds for average monthly views and for the updated data set
average_daily_views <- readRDS(paste(working_dir, "data/daily_average_views_10-languages.rds", sep = "")) # daily average views
average_daily_views_updated <- readRDS(paste(working_dir, "data/daily_average_views_10-languages_updated.rds", sep = "")) # daily average views updated

average_daily_views_new <- average_daily_views

# bind together the old two sets of views
for(i in 1:length(average_daily_views_new)){
  for(j in 1:length(average_daily_views_new[[i]])){
    average_daily_views_new[[i]][[j]] <- rbind(average_daily_views[[i]][[j]], average_daily_views_updated[[i]][[j]])
  }
}

# languages for views
languages <- c('^es_', '^fr_', '^de_', '^ja_', '^it_', '^ar_', '^ru_', '^pt_', '^zh_', '^en_')

#taxa of interest
taxa_ls <- c('actinopterygii', 'amphibia', 'aves', 'insecta', 'mammalia', 'reptilia')

# read in all the files for the real-time downloads
average_daily_views_real_time <- list()
average_daily_views_real_time_agg <- list()

# read in each of the new real-time files, binding multiple reads of each taxa/language combination for multiple months
for(i in 1:length(languages)){
  for(j in 1:length(taxa_ls)){
  average_daily_views_real_time[[j]] <- lapply(paste(working_dir, "data/real_time_views/species_views/",
                                                   grep(taxa_ls[j], list.files("data/real_time_views/species_views", pattern = languages[i]), value = TRUE), sep = ""), FUN = read.csv) %>%
    rbindlist() %>%
    select(-ï..) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    select(article, q_wikidata, year, month, av_views, date) %>%
    mutate(year = as.character(year)) %>%
    mutate(month = as.character(substr(date, start = 6, stop = 7)))
  }
  
  average_daily_views_real_time_agg[[i]] <- average_daily_views_real_time
  
}

# bind together all the real time data with the old view data, and filter out NA timestamps
for(i in 1:length(average_daily_views_new)){
  for(j in 1:length(average_daily_views_new[[i]])){
    average_daily_views_new[[i]][[j]] <- rbind(average_daily_views_new[[i]][[j]], average_daily_views_real_time_agg[[i]][[j]]) %>%
      filter(!is.na(date))
  }
}

# read in packages and data for each parallel session
clusterEvalQ(cl, {
  
  # set extra library path fro when running from Python
  .libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )
  
  # set working directory for each cluster
  working_dir <- "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/"
  
  # set up the packages required
  library(dplyr)
  library(data.table)
  library(mgcv)
  
  # read in additional functions
  source(paste(working_dir, "R/00_functions.R", sep = ""))

})

# set up the function for calculating trends
run_SAI_change <- function(views){
  
  browser()
  
  # arrange views by date
  views <- views %>%
    group_by(q_wikidata) %>%
    arrange(date) %>%
    ungroup()
  
  # convert to wide format
  views_wide <- tidyr::pivot_wider(views, 
                                   names_from = c(year, month), 
                                   values_from = av_views, 
                                   id_cols=c(year, month, av_views, q_wikidata))

  # remove any rows with NA and add 1 for following function
  views_wide <- views_wide[complete.cases(views_wide), ]

  # model each row with a GAM
  views_gammed <- gam_fn(views_wide)
  
  # convert to rates of change
  # if you do not want to limit log rates of change to [-1,1] (LPI default) set limiter=FALSE
  views_lambdas_list <- species_lambdas_fn(views_gammed,
                                           limiter=TRUE)

  # convert list of lambdas to data frame
  views_lambdas_dataframe <- do.call(rbind, views_lambdas_list)
  
  return(views_lambdas_dataframe)
  
}

SAI_trends <- list()

# iterate through each class/lamgauge combo
system.time({
  for(i in 1:length(average_daily_views_new)){
  
    SAI_trends[[i]] <- parLapply(cl, average_daily_views_new[[i]], fun = run_SAI_change)
    
    print(i)
  
  }
})
 
stopCluster(cl)

saveRDS(SAI_trends, paste(working_dir, "outputs/species_trends_updated_2.rds", sep = ""))

write.csv(data.frame(x = 1), "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/blah_1.csv")

