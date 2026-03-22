#!/usr/bin/env Rscript

# set extra library path fro when running from Python
.libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )

# set working directory for base corr
working_dir <- "C:/Users/josem4/Documents/real_time_SAI/"

# potentially add script to separate between smoothed and non-smoothed lambdas
# read in required packages
library(data.table)
library(dplyr)
library(boot)
library(forcats)
library(aws.s3)

# each of these csv reads needd to be replaced by a call to AWS, eventually to SQL database
s3BucketName <- "speciesawarenessindex-rc"

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
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", 
             "magnoliopsida", "liliopsida", "pinopsida", "cycadopsida", "polypodiopsida", "gnetopsida",
             "random_data")

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

# calculate the average lambda and add language/class columns, just for complete years
series_start <- head(date_vec[grepl("_01", date_vec)], 1)
series_end <- tail(date_vec[grepl("_01", date_vec)], 1)

# calculate the average lambda and add language/class columns
avg_lambdas <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  avg_lambdas[[i]] <- lapply(smoothed_adjusted_lamda[[i]], average_lambda, series_start = series_start, series_end = series_end)
}

# assign final column for languae
for(i in 1:length(avg_lambdas)){
  for(j in 1:length(avg_lambdas[[i]])){
    avg_lambdas[[i]][[j]]$language <- languages[[i]]
    avg_lambdas[[i]][[j]]$taxa <- classes[[j]]
  }
}

# bind all the lambda files into a single dataframe into a single dataframe
final_bound <- list()
for(i in 1:length(avg_lambdas)){
  final_bound[[i]] <- rbindlist(avg_lambdas[[i]], use.names = TRUE)
}

# rbind together the final dataframes
final_bound <- rbindlist(final_bound, use.names = TRUE) %>%
  mutate(taxa = factor(taxa)) %>%
  mutate(language = factor(language)) %>%
  rename("q_wikidata" = "data_file[, 1]")

## approach using language and class as fixed effects
model_3 <- lm(av_lambda ~ taxa * language, data = final_bound)

# set up prediction data
predicted_values_interaction <- predict(model_3, final_bound, se.fit = TRUE)

# create new dataframe for interactions
final_bound_interaction <- final_bound
final_bound_interaction$predicted_values <- predicted_values_interaction$fit
final_bound_interaction$predicted_values_se <- predicted_values_interaction$se.fit

# select unique dataframe with predicted values
fin_frame_6 <- final_bound_interaction %>%
  dplyr::select(taxa, language, predicted_values, predicted_values_se) %>%
  unique()

# add labels for factors, sort by predicted value for language and class, and then plot
class_language_models <- fin_frame_6 %>%
  mutate(taxa = factor(taxa, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia",
                                        "magnoliopsida", "liliopsida", "pinopsida", "cycadopsida", "polypodiopsida", "gnetopsida"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles",
                                             "Magnoliopsida", "Liliopsida", "Pinopsida", "Cycadopsida", "Polypodiopsida", "Gnetopsida"))) %>%
  mutate(language = factor(language, levels = c("ar", "fr", "zh", "en", "de", "es", "it", "ja", "pt" , "ru"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(taxa = fct_reorder(taxa, -predicted_values, median)) %>%
  mutate(language = fct_reorder(language, -predicted_values, median))

s3write_using(class_language_models, FUN = saveRDS, object = "class_language_change_2.rds", bucket = s3BucketName)

write.csv(data.frame(x = 1), paste(working_dir, "blah_5.csv"))

