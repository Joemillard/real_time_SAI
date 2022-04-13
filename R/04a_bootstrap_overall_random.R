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
working_dir <- "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/"

# read in the random view trends
language_views <- readRDS(paste(working_dir, "outputs/random_trends_updated.rds", sep = ""))

# add initial value of 1 to each random page
for(i in 1:length(language_views)){
  language_views[[i]] <- data.frame(append(language_views[[i]], list(X2015_07 = 1), after = match("q_wikidata", names(language_views[[i]]))))
}

# set up vector of column names
date_vec <- c(colnames(language_views[[1]][,2:76]))

# set up language vector
language_vec <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")

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

# function for boostrapping the create_lpi function for each lambda, and generating a 95 % confidence interval
run_each_group <- function(lambda_files, random_trend){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 1000)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trend
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)
}

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
saveRDS(lpi_trends_adjusted, paste(working_dir, "outputs/overall_random_updated_2.rds"))
