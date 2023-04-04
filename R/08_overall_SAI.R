# potentially add script to separate between smoothed and non-smoothed lambdas
# set extra library path fro when running from Python
.libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )

# set working directory for base corr
working_dir <- "C:/Users/josem4/Documents/real_time_SAI/"

# read in required packages
library(data.table)
library(dplyr)
library(boot)
library(forcats)
library(aws.s3)

# each of these csv reads needd to be replaced by a call to AWS, eventually to SQL database
s3BucketName <- "speciesawarenessindex"

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
    rename("q_wikidata" = "data_file[, 1]") %>%
    reshape2::melt(id = c("q_wikidata", "language")) %>%
    mutate(variable = as.character(variable)) %>%
    group_by(q_wikidata, variable) %>%
    summarise(mean_val = mean(value)) %>%
    ungroup()
  return(data_fin)
}

# rbindlist all lambda together and calculate averge for each species across languages
merge_species <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  merge_species[[i]] <- rbindlist(smoothed_adjusted_lamda[[i]], use.names = TRUE) %>%
    mutate(language = languages[i])
}

# merge all the lambda files, and calc average across each q_wikidata
merge_species <- rbindlist(merge_species, use.names = TRUE) %>% 
  wiki_average()

# reshape lambda files back into year rows, and then split into separate taxonomic classes
all_lambdas <- reshape2::dcast(merge_species, q_wikidata  ~ variable)

# run the boostrapping of trends for all lambda, and adjust for the random of that language
lpi_trends_adjusted <- run_each_group(all_lambdas, random_trend = date_vec)

# collapse together the average lambda at each start point for ecah class, add last row for value 57, and then stick LPI values back on
lpi_trends_adjusted <- lpi_trends_adjusted %>%
  mutate(Year = paste(Year, "_01", sep = "")) %>%
  mutate(Year = gsub("X", "", Year)) %>%
  mutate(Year = as.Date(Year, "%Y_%m_%d"))

# save rds locally for python code and to aws for shiny app
write.csv(lpi_trends_adjusted, paste(working_dir, "outputs/overall_2.csv", sep = ""), row.names=FALSE)
s3write_using(lpi_trends_adjusted, FUN = saveRDS, object = "overall_2.rds", bucket = s3BucketName)

write.csv(data.frame(x = 1), paste(working_dir, "blah_7.csv"))
