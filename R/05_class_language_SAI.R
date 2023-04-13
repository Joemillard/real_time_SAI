#!/usr/bin/env Rscript

# set extra library path fro when running from Python
.libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )

# set working directory for base corr
working_dir <- "C:/Users/josem4/Documents/real_time_SAI/"

# potentially add script to separate between smoothed and non-smoothed lambdas
# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)
library(aws.s3)

# source the functions R script
source(paste(working_dir, "R/00_functions.R", sep = ""))

# each of these csv reads needd to be replaced by a call to AWS, eventually to SQL database
s3BucketName <- "speciesawarenessindex-new"

# read in each of the secret keys hosted online
AWS_ACCESS_KEY_ID <- read.table(paste(working_dir, "R/app/AWS_ACCESS_KEY_ID.txt", sep = ""))
AWS_SECRET_ACCESS_KEY <- read.table(paste(working_dir, "R/app/AWS_SECRET_ACCESS_KEY.txt", sep = ""))
AWS_DEFAULT_REGION <- read.table(paste(working_dir, "R/app/AWS_DEFAULT_REGION.txt", sep = ""))

# set system environment for each of AWS keys
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION)

# source the functions R script
source(paste(working_dir, "R/00_functions.R", sep = ""))

# script for pollinator models using new language data
# read in the random rds file
species_trends_updated <- readRDS(paste(working_dir, "outputs/species_trends_updated_2.rds", sep = ""))

# add initial value of 1 to each species page
for(i in 1:length(species_trends_updated)){
  for(j in 1:length(species_trends_updated[[i]])){
    species_trends_updated[[i]][[j]] <- data.frame(append(species_trends_updated[[i]][[j]], list(X2015_07 = 1), after = match("q_wikidata", names(species_trends_updated[[i]][[j]]))))
  }
}

# set up vector of column names
date_vec <- c(colnames(species_trends_updated[[1]][[1]][,2:ncol(species_trends_updated[[1]][[1]])]))

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")

# read in the lambda files 
random_trend <- readRDS(paste(working_dir, "outputs/overall_random_updated_2.rds", sep = ""))

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$language <- languages[i]
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:length(date_vec)])))
}

# string for pollinating classes, plus random
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random_data")

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(species_trends_updated)){
  for(j in 1:length(species_trends_updated[[i]])){
    data_file <- species_trends_updated[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1], sweep(data_file[, 2:ncol(data_file)], 2, random_trend[[i]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

#### additional smoothing of the random adjusted indices

# smooth the adjusted random lambda for each species
# iterate through all the articles of that class/language
# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

###

# reassign correct names for each element in list
names(smoothed_adjusted_lamda) <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en") 
for(i in 1:length(smoothed_adjusted_lamda)){
  names(smoothed_adjusted_lamda[[i]]) <- (c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"))
}

# run the boostrapping of trends for each lambda
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  for(j in 1:length(smoothed_adjusted_lamda[[i]])){
    lpi_trends_adjusted[[j]] <- run_each_group(smoothed_adjusted_lamda[[i]][[j]], date_vec) %>%
      mutate(taxa = classes[j])
    
  }
  
  # bind together the trends for that language
  bound_trends[[i]] <- rbindlist(lpi_trends_adjusted, use.names = TRUE) %>%
    mutate(language = languages[i])
}

# bind together the trend for all languages
fin_bound_trends <- rbindlist(bound_trends, use.names = TRUE)

# amend data frame for plotting
class_language <- fin_bound_trends %>% 
  mutate(Year = paste(Year, "_01", sep = "")) %>%
  mutate(Year = gsub("X", "", Year)) %>%
  mutate(Year = as.Date(Year, "%Y_%m_%d")) %>%
  mutate(taxa = factor(taxa, levels = c("reptilia", "actinopterygii", "mammalia", "aves", "insecta", "amphibia"),
                       labels = c("Reptiles", "Ray finned fishes", "Mammals", "Birds", "Insects", "Amphibians"))) %>%
  mutate(language = factor(language, levels = c("ar", "zh", "en", "fr", "de", "it", "ja", "pt", "ru", "es"),
                           labels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish")))

# save file for Shiny app - this file needs to be written SQL database
#saveRDS(class_language, paste(working_dir, "outputs/shiny_outputs/class_language_2.rds", sep = ""))
s3write_using(class_language, FUN = saveRDS, object = "class_language_2.rds", bucket = s3BucketName)

write.csv(data.frame(x = 1), paste(working_dir, "blah_4.csv"))

