# potentially add script to separate between smoothed and non-smoothed lambdas
# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)
library(reshape2)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
species_trends <- readRDS(here::here("outputs/species_trends.rds"))

# set up vector of column names
date_vec <- c(colnames(species_trends[[1]][[1]][,3:58]), "2020_03")

# add initial value of 1 to each species page
for(i in 1:length(species_trends)){
  for(j in 1:length(species_trends[[i]])){
    species_trends[[i]][[j]] <- data.frame(append(species_trends[[i]][[j]], list(X2015_07 = 1), after = match("SpecID", names(species_trends[[i]][[j]]))))
    colnames(species_trends[[i]][[j]]) <- c("q_wikidata", "SpecID", date_vec)
  }
}
# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")

# read in the lambda files 
random_trend <- readRDS(here::here("outputs/overall_random.rds"))

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$language <- languages[i]
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:57])))
}

# string for pollinating classes, plus random
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random_data")

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(species_trends)){
  for(j in 1:length(species_trends[[i]])){
    data_file <- species_trends[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1:2], sweep(data_file[, 3:ncol(data_file)], 2, random_trend[[i]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

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

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  
  # remove na rows
  lambdas_new <- lambdas[complete.cases(lambdas), ]
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas_new[, 3:ncol(lambdas_new)]
  this_lambdas <- lambda_data[ind, ]
  mean_ann_lambda <- colMeans(this_lambdas, na.rm = TRUE)
  trend <- cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
smooth_all_groups <- function(data_file){
  
  # set up an empty list for smoothed values
  smoothed_indices <- list()
  
  # smooth the series for each row (species)
  for(i in 1:nrow(data_file)){
    smoothed_indices[[i]] <- smooth_series(X = as.numeric(as.vector(data_file[i, 4:ncol(data_file)])))
    smoothed_indices[[i]] <- create_lambda(smoothed_indices[[i]])
  }
  
  smoothed_lambda <- as.data.frame(do.call(rbind, smoothed_indices))
  
  # add back in the original column names
  colnames(smoothed_lambda) <- colnames(data_file)[3:ncol(data_file)]
  
  # bind the adjusted smoothed lambda back onto the first four columns
  smoothed_lambda <- cbind(data_file[,1:2], smoothed_lambda)
  
  return(smoothed_lambda)
  
}

# function for boostrapping the create_lpi function for each lambda, and generating a 95 % confidence interval
run_each_group <- function(lambda_files, random_trend){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 100)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trend
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)
}

# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

###
# function for binding all the lambdas together and calculate average for each q_wikidata
wiki_average <- function(data_file){
  data_fin <- data_file %>%
    reshape2::melt(id = c("q_wikidata", "SpecID", "language")) %>%
    mutate(variable = as.character(variable)) %>%
    group_by(q_wikidata, variable) %>%
    summarise(mean_val = mean(value)) %>%
    ungroup()
  return(data_fin)
}

# rbindlist all lambda together and calculate averge for each species across languages
merge_species <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  merge_species[[i]] <- rbindlist(smoothed_adjusted_lamda[[i]]) %>%
    mutate(language = languages[i])
}

# merge all the lambda files, and calc average across each q_wikidata
merge_species <- rbindlist(merge_species) %>% 
  wiki_average()

# reshape lambda files back into year rows, and then split into separate taxonomic classes
all_lambdas <- reshape2::dcast(merge_species, q_wikidata  ~ variable)

# run the boostrapping of trends for all lambda, and adjust for the random of that language
lpi_trends_adjusted <- run_each_group(all_lambdas, random_trend = date_vec)

# collapse together the average lambda at each start point for ecah class, add last row for value 57, and then stick LPI values back on
lpi_trends_adjusted <- lpi_trends_adjusted %>%
  mutate(Year = paste(Year, "_01", sep = "")) %>%
  mutate(Year = as.Date(Year, "%Y_%m_%d")) 


saveRDS(lpi_trends_adjusted, "outputs/shiny_outputs/overall.rds")



