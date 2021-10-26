# This code file contains functions for joecode1.1
# Functions were developed by Shawn Dove,
# based on parts of the rlpi package available at
# https://github.com/Zoological-Society-of-London/rlpi.
#
# Joe: feel free to change the above text. It's not meant to be official.

# functions ----

## function to interpolate missing population data using the chain method
chain_fn <- function(pop_data_culled, c, m_colnames, silent=FALSE) {
  
  # Check for GAM quality status column. If it exists, then GAMs have already
  # been performed, and the function will only interpolate populations that
  # failed the GAM quality check.
  if ("gqfail" %in% colnames(pop_data_culled)) {
    
    POSTGAM <- TRUE
    
  } else {
    
    POSTGAM <- FALSE
    
  }
  
  # This code returns the input as is. It's here to prevent the function from
  # breaking or returning NaNs when it encounters LPI categories without data.
  if(nrow(pop_data_culled)==0) {
    
    warning("The dataset was empty, so nothing has been done.")
    
    return(pop_data_culled)
    
  }
  
  ## Interpolation
  
  # create a matrix to hold the interpolated time series
  new.pop_data <- as.data.frame(matrix(NA, nrow = nrow(pop_data_culled[,1:c]), ncol = ncol(pop_data_culled[,1:c])))
  
  # create a vector to hold the rows
  rowsmat <- vector()
  
  # create a vector to record whether time series are copied or interpolated
  copied <- vector()
  
  # store the row names in a vector
  # this was added to avoid problems with row names in tibbles
  rownames_vec <- rownames(pop_data_culled)
  
  # create a counter to track how many populations have been interpolated
  counter1 <- 1
  
  # begin the interpolation loop
  for (i in 1:nrow(pop_data_culled)) {
    
    # put a single time series (row) into a vector
    new_pop_counts <- as.matrix(pop_data_culled[i,m_colnames])
    
    # put the row name of the time series into a vector
    #rownum <- rownames(pop_data_culled[i,])
    rownum <- rownames_vec[i] # this is to avoid problems with tibbles
    
    # check if GAMs have already been performed
    if (POSTGAM==TRUE) {
      
      # if so, check if this population passed the GAM quality check
      if (pop_data_culled$gqfail[i]==0) {
        
        # copy the time series into the new matrix
        new.pop_data[i,] <- new_pop_counts
        
        # put the row number of the time series into the row numbers vector
        rowsmat[i] <- rownum
        
        # if (silent==FALSE) {
        
        #   print(paste("copied population ", counter1, sep=""))
        
        # }
        
        # increase the counter
        counter1 <- counter1 + 1
        
        next
        
      }
      
    }
    
    # check if there are any zeros
    if (length(which(new_pop_counts==0))>0) {
      
      # check if all non-NA observations are zero
      if (mean(new_pop_counts[which(!is.na(new_pop_counts))])==0) {
        
        # if so, set zero adjust to a very small value
        zero_adjust <- 1e-17
        
      } else {
        
        # otherwise, calculate 1% of the mean of the observed values (excluding zeros)
        # if there are any zeros, this will be added to every observation to avoid issues with log of zero
        zero_adjust <- 0.01 * mean(new_pop_counts[which(new_pop_counts>0)], na.rm=TRUE)
        
      }
      
      # add the zero_adjust value
      new_pop_counts = new_pop_counts + zero_adjust
      
    }
    
    # get the missing counts from the time series
    pop_count_blanks <- new_pop_counts[,which(is.na(new_pop_counts))]
    
    # the lines below are a fix for a problem that occurs when there is only a single NA value
    new_pop_count_names <- as.integer(colnames(new_pop_counts))
    names(pop_count_blanks) <- new_pop_count_names[which(new_pop_counts %in% pop_count_blanks)]
    
    # if there are no missing counts, copy the time series directly and move on
    if (length(pop_count_blanks) == 0) {
      
      # copy the time series into the new matrix
      new.pop_data[i,] <- new_pop_counts
      
      # put the row number of the time series into the row numbers vector
      rowsmat[i] <- rownum
      
      # put 1 into the copied vector to record that this time series was not interpolated
      copied[i] <- 1
      
      #  if (silent==FALSE) {
      
      #    print(paste("copied population ", counter1, sep=""))
      
      #  }
      
      # increase the counter
      counter1 <- counter1 + 1
      
      next
      
    }
    
    # if there are 6 or more non-missing counts...
    if (length(new_pop_counts[,which(!is.na(new_pop_counts))]) >= 6) {
      
      # check if GAMs have already been performed
      if (POSTGAM==FALSE) {
        
        # if not, copy the time series into the new matrix
        new.pop_data[i,] <- new_pop_counts
        
        # put the row number of the time series into the row numbers vector
        rowsmat[i] <- rownum
        
        # put 1 into the copied vector to record that this time series was not interpolated
        copied[i] <- 1
        
        # if (silent==FALSE) {
        
        #   print(paste("copied population ", counter1, sep=""))
        
        # }
        
        # increase the counter
        counter1 <- counter1 + 1
        
        next
        
      }
      
    }
    
    # get the non-missing counts
    pop_count_filled <- new_pop_counts[,which(!is.na(new_pop_counts))]
    
    # get the column numbers of the missing counts
    pop_count_blank_cols <- which(new_pop_counts %in% pop_count_blanks)
    
    # get the column numbers of the non-missing counts
    pop_count_filled_cols <- which(new_pop_counts %in% pop_count_filled)
    
    # get the years of the missing counts
    pop_count_blank_years <- as.integer(names(pop_count_blanks))
    
    # get the years of the non-missing counts
    pop_count_filled_years <- as.integer(names(pop_count_filled))
    
    # get the year of the first non-missing count
    first_filled_year <- min(pop_count_filled_years)
    
    # get the year of the last non-missing count
    last_filled_year <- max(pop_count_filled_years)
    
    # get the column number of the first non-missing count
    first_filled_col <- min(pop_count_filled_cols)
    
    # get the column number of the last non-missing count
    last_filled_col <- max(pop_count_filled_cols)
    
    # make a vector of all columns between and including the first to the last non-missing counts of the time series
    # this will be used to determine years which must be interpolated. Projection will be done later.
    actual_ts_cols <- first_filled_col:last_filled_col
    
    # make a vector of years that match the columns
    actual_ts_years <- first_filled_year:last_filled_year
    
    # make a vector of columns to be interpolated by matching the above columns vector with the non-missing counts
    actual_ts_blank_cols <- pop_count_blank_cols[pop_count_blank_cols %in% actual_ts_cols]
    
    # get the years of the missing counts within the time series period
    actual_ts_blank_years <- actual_ts_years[actual_ts_years %in% pop_count_blank_years]
    
    # get column numbers of existing counts within the time series period
    actual_ts_filled_cols <- actual_ts_cols[actual_ts_cols %in% pop_count_filled_cols]
    
    # get existing counts for use in interpolation
    actual_ts_filled_vals <- new_pop_counts[,actual_ts_filled_cols]
    
    # get the years of the non-missing counts within the period of the time series
    actual_ts_filled_years <- actual_ts_years[actual_ts_years %in% pop_count_filled_years]
    
    #interpolate to fill in missing values within the time series period (not projected), using log-linear interpolation
    pop_interp_temp <- approx(actual_ts_filled_years, log(actual_ts_filled_vals), actual_ts_blank_years)
    
    # back convert from log
    pop_interp <- exp(pop_interp_temp$y)
    
    # create a counter for use when adding the interpolated values into the time series
    counter2 <- 1
    
    # loop for adding the interpolated values into the time series
    for (j in actual_ts_blank_cols) {
      
      # add interpolated values. counter is used for pop_interp because it has fewer values than new_pop_counts
      new_pop_counts[,j] <- pop_interp[counter2]
      
      # increase the counter each time a value is added to new_pop_counts
      counter2 <- counter2 + 1
      
    }
    
    # put the interpolated time series into the new matrix
    new.pop_data[i,] <- as.matrix(new_pop_counts)
    
    # put the row number of the time series into the row numbers vector
    rowsmat[i] <- rownum
    
    # put 0 into the copied vector to record that this time series was interpolated
    copied[i] <- 0
    
    if (silent==FALSE) {
      
      print(paste("completed interpolation of population ", counter1, sep=""))
      
    }
    
    counter1 <- counter1 + 1
    
  }
  
  # add id tags back
  new.pop_data[,(c+1):length(pop_data_culled)] <- pop_data_culled[,(c+1):length(pop_data_culled)]
  
  # put column names into the new matrix
  colnames(new.pop_data) <- colnames(pop_data_culled)
  
  # put row names into the new matrix
  rownames(new.pop_data) <- rowsmat
  
  # check if GAMs have already been performed
  if (POSTGAM==FALSE) {
    
    # if not, add "copied" vector as a column to show the gam function which time series to ignore
    new.pop_data$copied <- copied
    
  }
  
  return(new.pop_data)
  
}

# function to GAM population time series
gam_fn <- function(new.pop_data, c, m_colnames, silent=FALSE) {
  
  # create a list to put resampled populations into
  gam_poplist <- list()
  
  # create a vector of population IDs
  pop_ids <- new.pop_data$PopID
  
  # create a vector of row numbers
  rows <- 1:nrow(new.pop_data)
  
  # create vector to record populations which fail GAM quality check
  gqfail <- vector()
  
  # reorganize the data into a long format
  trim.mat <- matrix(NA, nrow = nrow(new.pop_data) * c, ncol = 3)
  
  # name columns
  colnames(trim.mat) <- c("population", "year", "count")
  
  # convert to data frame
  trim.mat <- as.data.frame(trim.mat)
  
  # fill columns with data
  trim.mat[,1] <- as.factor(rep(pop_ids, each = c)) # population ID
  
  trim.mat[,2] <- as.numeric(rep(m_colnames, times = nrow(new.pop_data))) # year
  
  trim.mat[,3] <- as.numeric(as.vector(t(new.pop_data[m_colnames])))  # count
  
  # create copied vector to check which populations have already been interpolated
  copied <- new.pop_data$copied
  
  # create vector of rows which have been interpolated
  copied_rows <- which(copied[rows]==0)
  
  # create vector of rows which have not been interpolated
  gam_rows <- which(copied[rows]==1)
  
  for (row in copied_rows) {
    
    # copy population into completed list, without the "copied" column
    gam_poplist[[row]] <- new.pop_data[row,1:c]
    
    # flag the GAM quality status as pass (because pop was not GAM'd)
    gqfail[row] <- 0
    
    if (silent==FALSE) {
      
      print(paste("copied population ", row, sep="")) 
      
    }
    
  } 
  
  
  for (row in gam_rows) {
    
    # Get pop data
    pop_data = subset(trim.mat, population == pop_ids[row])
    
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
      
      # flag the GAM quality status as a pass
      gqfail[row] <- 0
      
      # if GAM fails the quality check ...
    } else {
      
      # copy the original data for using the chain method later
      gam_poplist[[row]] <- new.pop_data[row,1:c]
      
      # flag the GAM quality status as a fail
      gqfail[row] <- 1
      
      if (silent==FALSE) {
        
        print(paste0("GAM of population ", row, " failed quality check.", sep=""))
        
      }
      
      next
      
    }
    
    # create matrix to hold GAM'd population
    pred.a <- matrix(NA, nrow=1, ncol=c)
    
    # add column names
    colnames(pred.a) = paste(m_colnames)
    
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
    colnames(pred.a) <- colnames(new.pop_data[,m_colnames])
    
    # add GAM'd population to list
    gam_poplist[[row]] <- pred.a
    
    if (silent==FALSE) {
      
      print(paste("completed GAM of population ", row, sep=""))
      
    }
    
  }
  
  # convert from list to data frame
  gam_popmat <- do.call(rbind, gam_poplist)
  
  # if there are no populations...
  if (is.null(gam_popmat)) {
    
    # output as a single row of NAs
    gam_popmat <- as.data.frame(matrix(NA, nrow=1, ncol=(ncol(new.pop_data)-1)))
    
    # flag the quality check status as pass, since there is nothing more to be done
    gqfail <- 0
    
  }
  
  # add extra columns back in from the original data frame, minus the copied status
  gam_popmat[,(c+1):(length(new.pop_data)-1)] <- new.pop_data[,(c+1):(length(new.pop_data)-1)]
  
  # add column names back in
  colnames(gam_popmat) <- colnames(new.pop_data[,1:(length(new.pop_data)-1)])
  
  # add the GAM quality check status to the data frame
  gam_popmat$gqfail <- gqfail
  
  return(gam_popmat)
  
}


# function to create species lambdas from interpolated populations
species_lambdas_fn <- function(pop_data, c, limiter=FALSE, silent=FALSE) {
  
  pop_data2 <- pop_data
  
  if (is.null(pop_data$SpecID)) {
    
    pop_data2$SpecID <- pop_data$PopID
    
  }
  
  # make a vector of all unique species IDs in the sample
  spec_ids <- unique(pop_data2$SpecID)
  
  # make a vector of population IDs in the sample
  pop_ids <- pop_data2$PopID
  
  # create list to hold the species indices
  spec_lambdas.list <- list()
  
  # start a counter to iterate species
  counter1 <- 1
  
  # if there is no population data...
  if(!any(!is.na(pop_data2[,1:c])) & nrow(pop_data2)==1) {
    
    # get data
    sample_mat5 <- as.data.frame(pop_data2[,1:c])
    
    if (is.null(pop_data$SpecID)) {
      
      # add a population ID column
      sample_mat5$PopID <- NA
      
    } else {
      
      # add a species ID column
      sample_mat5$SpecID <- NA
      
    }
    
    # put into list format
    spec_lambdas.list[[counter1]] <- sample_mat5
    
    print(paste("No population data."))
    
    return(spec_lambdas.list)
    
  }
  
  # loop to create species indices
  for (spec in spec_ids) {
    
    # select all resampled populations that belong to a particular species
    spec_popdata <- subset(pop_data2, SpecID==spec)
    
    q <- spec_popdata$q_wikidata
    
    if (is.null(pop_data$SpecID)) {
      
      # get population ID
      pop <- spec_popdata$PopID
      
    }
    
    # get species data
    sample_mat1.2 <- as.matrix(spec_popdata[,1:c])
    
    # if there is only one population...
    if (nrow(spec_popdata)==1) {
      
      # convert to lambda values
      sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
      sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
      sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
      
      # restructure as data frame
      sample_mat1.7 <- as.data.frame(t(sample_mat1.5))
      
      # convert lambda values to log10
      sample_mat1.8 <- log10(sample_mat1.7)
      
      if (limiter==TRUE) {
        
        # limit lambda values to -1:1 on the log scale
        sample_mat5 <- as.data.frame(t(apply(sample_mat1.8, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})))
        
      } else {
        
        # update variable name
        sample_mat5 <- as.data.frame((sample_mat1.8))
        
      }
      
      # add years as column names
      colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
      
      if (is.null(pop_data$SpecID)) {
        
        # add a population ID column
        sample_mat5$PopID <- pop
        
      } else {
        
        # add a species ID column
        sample_mat5$SpecID <- spec
        
      }
      
    } else {
      
      # convert to lambda values
      sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
      sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
      sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
      
      # convert time series values to log10 scale
      sample_mat1.7 <- log10(sample_mat1.5)
      
      if (limiter==TRUE) {
        
        # limit all change values to -1:1 on the log scale)
        sample_mat1.8 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})
        
      } else {
        
        # update variable name
        sample_mat1.8 <- sample_mat1.7
        
      }
      
      # take mean of population lambda values at each time point
      sample_mat2 <- colMeans(sample_mat1.8, na.rm=TRUE)
      
      # convert NaN values to NA
      sample_mat2[is.nan(sample_mat2)] <- NA
      
      # transpose species lambdas as a matrix
      sample_mat5 <- t(as.matrix(sample_mat2))
      
      # add column names
      colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
      
      # add rownames
      rownames(sample_mat5) <- rownames(sample_mat2)
      
      # restructure as data frame
      sample_mat5 <- as.data.frame(sample_mat5)
      
      # add a species ID column
      sample_mat5$SpecID <- spec
      
    }
    
    sample_mat5$q_wikidata <- q
    
    # move id columns before year_month columns
    sample_mat5 <- relocate(sample_mat5, c(q_wikidata, SpecID))
    
    # add species lambdas to the list
    spec_lambdas.list[[counter1]] <- sample_mat5
    
    if (silent==FALSE) {
      
      print(paste("completed lambdas for species ", counter1, sep=""))
      
    }
    
    # increase counter value to keep track of the number of species completed
    counter1 <- counter1 + 1
    
  }
  
  return(spec_lambdas.list)
  
}

