## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# for parallel session
library(parallel)

# set up cores for parallel processing
cl <- makeCluster(detectCores())

# read in the rds for average monthly views and for the updated data set
average_daily_views <- readRDS("data/daily_average_views_10-languages.rds") # daily average views
average_daily_views_updated <- readRDS("data/daily_average_views_10-languages_updated.rds") # daily average views updated

for(i in 1:length(average_daily_views_new)){
  for(j in 1:length(average_daily_views_new[[i]])){
    average_daily_views_new[[i]][[j]] <- rbind(average_daily_views[[i]][[j]], average_daily_views_updated[[i]][[j]])
  }
}

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

# iterate througheach class/lamgauge combo
system.time({
  for(i in 1:length(average_daily_views)){
  
    SAI_trends[[i]] <- parLapply(cl, average_daily_views[[i]], fun = run_SAI_change)
    
    print(i)
  
  }
})
   
test_dat <- run_SAI_change(average_daily_views[[1]][[1]])
 
stopCluster(cl)

saveRDS(SAI_trends, "outputs/species_trends_updated.RDS")
