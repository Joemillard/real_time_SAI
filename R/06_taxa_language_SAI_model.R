# potentially add script to separate between smoothed and non-smoothed lambdas
# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)

# source the functions R script
source("R/00_functions.R")

# script for pollinator models using new language data
# read in the random rds file
species_trends_updated <- readRDS(here::here("outputs/species_trends_updated_2.rds"))

# add initial value of 1 to each species page
for(i in 1:length(species_trends_updated)){
  for(j in 1:length(species_trends_updated[[i]])){
    species_trends_updated[[i]][[j]] <- data.frame(append(species_trends_updated[[i]][[j]], list(X2015_07 = 1), after = match("q_wikidata", names(species_trends_updated[[i]][[j]]))))
  }
}

# set up vector of column names
date_vec <- c(colnames(species_trends_updated[[1]][[1]])[2:76])

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")

# read in the lambda files 
random_trend <- readRDS(here::here("outputs/overall_random_updated_2.rds"))

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$language <- languages[i]
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:75])))
}

# bind together and plot the random trends
random_trend_figure <- rbindlist(random_trend) %>%
  mutate(language = factor(language, levels = c("ar", "fr", "zh", "en", "de", "es", "it", "ja", "pt" , "ru"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = language)) +
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = language), alpha = 0.3) +
  scale_y_continuous("Random index", breaks = c(0.6, 1, 1.4, 1.8)) +
  facet_wrap(~language) +
  theme_bw() +
  theme(panel.grid = element_blank())

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
smooth_series <- function(X){
  
  # create index
  index <- cumprod(10^c(0, X))
  
  # smooth the index
  x_range <- 1:length(index)
  y.loess <- loess(index~x_range, span = 0.30)
  data_fin <- predict(y.loess, data.frame(x_range))
  return(data_fin)
}

# convert the index back to lambda
create_lambda <- function(X){
  lambda <- c(1, diff(log10(X)))
  return(lambda)
}

# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
smooth_all_groups <- function(data_file){
  
  # set up an empty list for smoothed values
  smoothed_indices <- list()
  
  # smooth the series for each row (species)
  for(i in 1:nrow(data_file)){
    smoothed_indices[[i]] <- smooth_series(X = as.numeric(as.vector(data_file[i, 3:ncol(data_file)])))
    smoothed_indices[[i]] <- create_lambda(smoothed_indices[[i]])
  }
  
  smoothed_lambda <- as.data.frame(do.call(rbind, smoothed_indices))
  
  # add back in the original column names
  colnames(smoothed_lambda) <- colnames(data_file)[2:ncol(data_file)]
  
  # bind the adjusted smoothed lambda back onto the first four columns and correct year
  smoothed_lambda <- cbind(data_file[,1], smoothed_lambda)
  
  return(smoothed_lambda)
  
}

# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

# calculate average for each row and reformat with language and class included
average_lambda <- function(data_file, taxa, series_start, series_end){
  
  # builds subset of columns to calculate average over
  data_fin <- data_file
  
  # select lambda columns
  data_subset <- data_fin[, (grep(series_start, colnames(data_fin))):grep(series_end, colnames(data_fin))]
  
  # remove NAs from mean calculation using rowMeans
  data_fin$av_lambda <- apply(data_subset, 1, mean, na.rm = TRUE)
  return(data_fin)
}

# calculate the average lambda and add language/class columns, just for complete years
series_start <- c("2016_01")
series_end <- c("2021_01")

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
  final_bound[[i]] <- rbindlist(avg_lambdas[[i]])
}

# rbind together the final dataframes
final_bound <- rbindlist(final_bound) %>%
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
  mutate(taxa = factor(taxa, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  mutate(language = factor(language, levels = c("ar", "fr", "zh", "en", "de", "es", "it", "ja", "pt" , "ru"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(taxa = fct_reorder(taxa, -predicted_values, median)) %>%
  mutate(language = fct_reorder(language, -predicted_values, median))

saveRDS(class_language_models, "outputs/shiny_outputs/class_language_change_2.rds")
