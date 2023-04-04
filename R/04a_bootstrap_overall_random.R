#!/usr/bin/env Rscript

# bootstrap the overall random trend

# set extra library path fro when running from Python
.libPaths(c( .libPaths(), "C:/Users/Joseph Millard/Documents/R/win-library/4.1") )

# read in packages
library(dplyr)
library(data.table)
library(boot)
library(ggplot2)

# set working directory for base corr
working_dir <- "C:/Users/josem4/Documents/real_time_SAI/"

# source the functions R script
source(paste(working_dir, "R/00_functions.R", sep = ""))

# read in the random view trends
language_views <- readRDS(paste(working_dir, "outputs/random_trends_updated.rds", sep = ""))

# add initial value of 1 to each random page
for(i in 1:length(language_views)){
  language_views[[i]] <- data.frame(append(language_views[[i]], list(X2015_07 = 1), after = match("q_wikidata", names(language_views[[i]]))))
}

# set up vector of column names
date_vec <- c(colnames(language_views[[1]][,2:ncol(language_views[[1]])]))

# set up language vector
language_vec <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(language_views)){
  lpi_trends_adjusted[[i]] <- run_each_group(language_views[[i]], date_vec) %>%
    mutate(language = language_vec[i]) %>%
    select(Year, LPI, LPI_lwr, LPI_upr) %>%
    rename("LPI_final" = "LPI") %>%
    rename("CI_low" = "LPI_lwr") %>%
    rename("CI_high" = "LPI_upr")
  
  rownames(lpi_trends_adjusted[[i]]) <- c()
}

# resave the average lambda for random views, according to bootstrap method used throughout
saveRDS(lpi_trends_adjusted, paste(working_dir, "outputs/overall_random_updated_2.rds", sep = ""))

write.csv(data.frame(x = 1), paste(working_dir, "blah_3.csv"))

