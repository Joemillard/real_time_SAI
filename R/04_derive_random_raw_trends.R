## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# for parallel session
library(parallel)
library(dplyr)

# read in the rds for total monthly views
average_daily_views <- readRDS("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/average_views/average_daily_views_random_10-languages.rds") # daily average views

# set up vectors of wiki project class to remove any animal species from the random data
wiki_proj <- paste(c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en"), "wiki", sep = "")
taxa_groups <- c("ACTINOPTERYGII", "AMPHIBIA", "AVES", "INSECTA", "MAMMALIA", "REPTILIA")

# read in the biodiversity pages
biodiversity_pages <- read.csv(here::here("data/all_iucn_titles.csv"), encoding = "UTF-8") %>%
  filter(site %in% wiki_proj) %>%
  filter(class_name %in% taxa_groups) %>%
  select(title, site, class_name) %>%
  unique() %>%
  mutate(site = factor(site, levels = wiki_proj)) %>%
  arrange(site)

# filter the biodiversity pages for the set of languages we're using, then split up, and sort by random languages list
split_biodiversity_pages <- split(biodiversity_pages, biodiversity_pages$site)

# filter all pages from random that are species pages
filter_species <- function(data_file, split_biodiversity_pages){
  data_fin <- anti_join(data_file, split_biodiversity_pages, by = c("article" = "title"))
  return(data_fin)
}

# merge the random species title with the all species list for each language to remove species from random - antijoin to remove those in both
for(i in 1:length(average_daily_views)){
  average_daily_views[[i]] <- filter_species(data_file = average_daily_views[[i]], 
                                             split_biodiversity_pages = split_biodiversity_pages[[i]]) %>%
    select(article, q_wikidata, year, month, av_views, date)
}

# set up cores for parallel processing
cl <- makeCluster(detectCores())

# read in packages and data for each parallel session
clusterEvalQ(cl, {
  
  # set up the packages required
  library(dplyr)
  library(data.table)
  library(mgcv)
  
  # read in additional functions
  source("R/00_functions.R")
  
})

# set up the function for calculating trends
run_SAI_change <- function(views){
  
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
  views_lambdas_dataframe <- do.call(rbind, views_lambdas_list)
  
  return(views_lambdas_dataframe)
  
}

# iterate through each class/langauge combo
system.time({
    random_trends <- parLapply(cl, average_daily_views, fun = run_SAI_change)
})

stopCluster(cl)

saveRDS(random_trends, "random_trends.RDS")
