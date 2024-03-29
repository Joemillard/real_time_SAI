# functions to create Generalized Additive Models of population time series and calculate monthly growth rates
# based on parts of the rlpi package available at
# https://github.com/Zoological-Society-of-London/rlpi.
#

## gam function ----

# function to GAM time series
gam_fn <- function(new.pop_data) {
  
  # create a list to put resampled populations into
  gam_poplist <- list()
  
  # create a vector of q_wikidata IDs
  q_wikidata <- new.pop_data$q_wikidata
  
  # save column names to restore later
  old_names <- colnames(new.pop_data)
  
  #rename columns to numeric
  colnames(new.pop_data) <- c("q_wikidata", seq(1, ncol(new.pop_data)-1))
  
  # convert to long format with reshape2 instead of tidyr
  data_long <- reshape2::melt(new.pop_data, id = c("q_wikidata")) %>%
    mutate(variable = as.numeric(variable)) %>%
    rename(year = variable) %>%
    rename(count = value) %>%
    mutate(count = as.numeric(count))
  
  # start counter to track completed rows
  counter <- 1
  
  # loop through q_wikidata IDs
  for (q in q_wikidata) {
    
    # Get data
    pop_data = subset(data_long, q_wikidata == q)
    
    # check if there are any zeros
    if (length(which(pop_data$count==0))>0) {
      
      # check if all non-NA observations are zero
      if (mean(pop_data$count[which(!is.na(pop_data$count))])==0) {
        
        # if so, set zero adjust to a very small value
        zero_adjust <- 1e-17
        
      } else {
        
        # otherwise, calculate 1% of the mean of the observed values (excluding zeros)
        # if there are any zeros, this will be added to every observation to avoid issues with log of zero
        zero_adjust <- 0.01 * mean(pop_data$count[which(pop_data$count>0)], na.rm=TRUE)
        
      }
      
      # take the log and add the zero_adjust value
      pop_data$log_popvalue = log(pop_data$count + zero_adjust)
      
    } else {
      
      # create zero_adjust value of 0 for later use
      zero_adjust <- 0
      
      # add log column
      pop_data$log_popvalue = log(pop_data$count)
      
    }
    
    # K is half of the number of non-NA values in pop_data$count (if it is an odd number, K will be rounded down)
    K = round(length(which(!is.na(pop_data$count)))/2)
    
    # Make GAM
    b <- gam(log_popvalue ~ s(year, k = K), data = pop_data)
    
    # check the model fit
    # first, get the residuals of the GAM model
    resid <- residuals(b)
    
    # change K to the full number of non-NA values in pop_data$count 
    K = length(which(!is.na(pop_data$count)))
    
    # set years for residuals
    resid.years <- pop_data$year[which(!is.na(pop_data$count))]
    
    # then GAM the residuals (using same GAM settings as LPI)
    resid.gam <- gam(resid ~ s(resid.years, k = K, bs = "cs"), gamma = 1.4)
    
    # finally, check whether the sum of the estimated degrees of freedom is close to 1
    if ((abs(sum(resid.gam$edf) - 1)) < 0.01) {
      
      # if GAM fails the quality check ...
    } else {
      
      # copy the original data with zero adjustment
      gam_poplist[[counter]] <- new.pop_data[counter,2:ncol(new.pop_data)] + zero_adjust
      
      # increment counter
      counter <- counter + 1
      
      # move to next row
      next
      
    }
    
    # create matrix to hold GAM'd population
    pred.a <- matrix(NA, nrow=1, ncol=ncol(new.pop_data)-1)
    
    # add column names
    colnames(pred.a) = paste(1:(ncol(new.pop_data)-1))
    
    # predict all values between the first and last observations using GAM
    startGAM <- min(which(!is.na(pop_data$count)))
    endGAM <- max(which(!is.na(pop_data$count)))
    pred.a[,startGAM:endGAM] <- t(predict(b, pop_data[startGAM:endGAM,]))
    
    # convert to matrix
    pred.a <- as.matrix(pred.a)
    
    # convert back to index values
    pred.a <- exp(pred.a)
    
    # convert any negative values to 0s
    pred.a <- ifelse(pred.a < 0, 0, pred.a)
    
    # convert to data frame
    pred.a <- as.data.frame(pred.a)
    
    # add column names
    colnames(pred.a) <- colnames(new.pop_data[,2:ncol(new.pop_data)])
    
    # add GAM'd population to list
    gam_poplist[[counter]] <- pred.a
    
    # increment counter  
    counter <- counter + 1
    
  }
  
  # convert from list to data frame
  gam_popmat <- do.call(rbind, gam_poplist)
  
  # if there are no populations...
  if (is.null(gam_popmat)) {
    
    # output as a single row of NAs
    gam_popmat <- as.data.frame(matrix(NA, nrow=1, ncol=(ncol(new.pop_data)-1)))
    
  }
  
  # restore q_wikidata column
  gam_popmat$q_wikidata <- q_wikidata
  
  #move q_wikidata column to the beginning
  gam_popmat <- relocate(gam_popmat, q_wikidata)
  
  #restore column names
  colnames(gam_popmat) <- old_names
  
  return(gam_popmat)
  
}

## species lambda function ----

# function to create species lambdas from interpolated populations
species_lambdas_fn <- function(pop_data, limiter=FALSE) {
  
  # create list to hold the lambdas
  lambdas.list <- list()
  
  # if there is nothing there...
  if(!any(!is.na(pop_data[,2:ncol(pop_data)])) & nrow(pop_data)==1) {
    
    # get data
    sample_mat5 <- as.data.frame(pop_data)
    
    # put into list format
    spec_lambdas.list[[1]] <- sample_mat5
    
    print(paste("No population data."))
    
    #return(spec_lambdas.list)
    return(pop_data)
    
  }
  
  # loop to create species indices
  for (row in 1:nrow(pop_data)) {
    
    # get row
    spec_popdata <- pop_data[row,]
    
    # store q_wikidata ID
    q <- spec_popdata$q_wikidata
    
    # get views and convert to matrix format
    sample_mat1.2 <- as.matrix(spec_popdata[,2:ncol(spec_popdata)])
    
    # convert to lambda values
    sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
    sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
    sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
    
    # restructure as data frame
    sample_mat1.7 <- as.data.frame(t(sample_mat1.5))
    
    # convert lambda values to log10
    sample_mat1.8 <- log10(sample_mat1.7)
    
    if (limiter==TRUE) {
      
      # limit lambda values to -1:1 on the log10 scale
      sample_mat5 <- as.data.frame(t(apply(sample_mat1.8, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})))
      
    } else {
      
      # update variable name
      sample_mat5 <- as.data.frame((sample_mat1.8))
      
    }
    
    sample_mat5$q_wikidata <- q
    
    # move id columns before year_month columns
    sample_mat5 <- relocate(sample_mat5, q_wikidata)
    
    # add lambdas to the matrix
    lambdas.list[[row]] <- sample_mat5
    
  }
  
  return(lambdas.list)
  
}

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
                                   id_cols=c(q_wikidata))
  
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

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas[, 3:ncol(lambdas)]
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
  boot_res <- data.frame("LPI" = dbi.boot$t0, "Year" = random_trend)
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)

}

# smooth the random adjusted species pages
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
  
  # bind the adjusted smoothed lambda back onto the first four columns
  smoothed_lambda <- cbind(data_file[,1], smoothed_lambda)
  
  return(smoothed_lambda)
  
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
