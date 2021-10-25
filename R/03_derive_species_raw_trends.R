## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# set up the packages required
library(ggplot2)
library(dplyr)
library(data.table)
library(mgcv)

# read in additional functions
source("R/00_functions.R")

# read in the rds for total monthly views
average_daily_views <- readRDS("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/average_views/daily_average_views_10-languages.rds") # daily average views

# this section is unimportant. it is just for navigating the tribble

# get names of languages from the tribble
languages <- names(average_daily_views)

# get names of classes from the tribble
classes <- names(average_daily_views[[1]])

views_lambdas_df <- list()

# main code ----
#for(i in 1:length(average_daily_views)){
  
  #views_lambdas_dataframe <- list()
  
 # for(j in 1:length(average_daily_views[[i]])){
  
    # wikipedia page views data for a particular language/class combination
    # specified by i and j
    views <- average_daily_views[[1]][[1]]
    
    # convert to wide format
    views_wide <- tidyr::pivot_wider(views, 
                              names_from = c(year, month), 
                              values_from = av_views, 
                              id_cols=c(year, month, av_views, q_wikidata))
    
    # save names of columns containing av_views values in a variable
    colnames_var <- colnames(views_wide[,2:length(views_wide)])
    
    # move the q_wikidata column to the end
    views_wide <- relocate(views_wide, 
                           q_wikidata, 
                           .after = last_col())
    
    # create a variable with temporary numeric column names
    m_colnames <- 1:(length(views_wide)-1)
    
    # save the number of numeric columns in a variable
    columns <- length(m_colnames)
    
    # replace column names in views_wide with temporary numeric column names
    colnames(views_wide) <- c(m_colnames, 
                              "q_wikidata")
    
    # add a unique population id
    views_wide$PopID <- seq_along(views_wide$q_wikidata)
    
    # add a unique species id
    views_wide$SpecID <- seq_along(views_wide$q_wikidata)
    
    # remove all time series that do not meet minimum length and observation # thresholds
    views_wide_culled <- cull_fn(views_wide, 
                                 count_thres=2, 
                                 min_ts_length=57, columns) %>%
      mutate(copied = 1)
    
    # GAM the population indices, checking the fit and marking those that dont fit well
    views_gammed <- gam_fn(views_wide_culled, 
                           columns, 
                           m_colnames)
    
    # log-linear interpolate populations that had poorly fitting GAMs
    views_complete <- chain_fn(views_gammed, 
                               columns, 
                               m_colnames)
    
    # put saved column names back in
    colnames(views_complete) <- c(colnames_var, 
                                  "q_wikidata", 
                                  "PopID", 
                                  "SpecID",
                                  "copied")
    
    # convert interpolated/imputed populations to rates of change
    # if you do not want to limit log rates of change to [-1,1] (LPI default) set limiter=FALSE
    views_lambdas_list <- species_lambdas_fn(views_complete, 
                                             columns, 
                                             limiter=TRUE)
    
    # convert list of lambdas to data frame
    views_lambdas_dataframe[[j]] <- do.call(rbind, views_lambdas_list)

  }
  
  views_lambdas_df[[i]] <- views_lambdas_dataframe
  
  print(languages[i])

}



















