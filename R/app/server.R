# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  library(dplyr)
  library(aws.s3)
  
  # each of these csv reads needd to be replaced by a call to AWS, eventually to SQL database
  s3BucketName <- "speciesawarenessindex-new"
  
  # read in each of the secret keys hosted online
  AWS_ACCESS_KEY_ID <- read.table("AWS_ACCESS_KEY_ID.txt")
  AWS_SECRET_ACCESS_KEY <- read.table("AWS_SECRET_ACCESS_KEY.txt")
  AWS_DEFAULT_REGION <- read.table("AWS_DEFAULT_REGION.txt")
  
  # set system environment for each of AWS keys
  Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
              "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
              "AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION)
  
  # read in each of the rds files from AWS
  class_language <- s3read_using(FUN = readRDS, bucket = s3BucketName, object = "class_language_2.rds")
  class_language_change <- s3read_using(FUN = readRDS, bucket = s3BucketName, object = "class_language_change_2.rds")
  class_SAI <- s3read_using(FUN = readRDS, bucket = s3BucketName, object = "class_trend_2.rds")
  overall_SAI <- s3read_using(FUN = readRDS, bucket = s3BucketName, object = "overall_2.rds")
  
  # need to make requests of our SAI API here, which pulls in a set of dataframes:
  # overall_SAI, class_SAI, class_language_SAI, class_language_change
  
  # plot of total production vulnerability for each country
  output$overall_SAI <-  renderPlot({
    
    # main text plot -- collapse together the average lambda at each start point for each class, add last row for value 57, and then stick LPI values back on
    overall_SAI %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.3) +
      geom_line(aes(x = Year, y = LPI)) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      scale_y_continuous(breaks = c(1.05, 1, 0.95, 0.9, 0.85, 0.8), labels = c("1.05","1", "0.95", "0.9", "0.85", "0.8")) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # plot of total production vulnerability for each country
  output$overall_SAI_plants <-  renderPlot({
    
    # main text plot -- collapse together the average lambda at each start point for each class, add last row for value 57, and then stick LPI values back on
    overall_SAI %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.3) +
      geom_line(aes(x = Year, y = LPI)) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      scale_y_continuous(breaks = c(1.05, 1, 0.95, 0.9, 0.85, 0.8), labels = c("1.05","1", "0.95", "0.9", "0.85", "0.8")) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # plot of total production vulnerability for each country
  output$class_SAI <-  renderPlot({
    
    # plot all the class level trends with point for whether increasing or decreasing
    class_SAI %>%
      filter(taxa %in% c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles")) %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = taxa), alpha = 0.3) +
      geom_line(aes(x = Year, y = LPI, colour = taxa), size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      scale_colour_manual("Taxonomic class", na.translate = F, values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_fill_manual("Taxonomic class", na.translate = F, values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_y_continuous(breaks = c(0.8, 1, 1.2), labels = c("0.8", "1", "1.2")) +
      facet_wrap(~taxa) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # plot of total production vulnerability for each country
  output$class_SAI_plants <-  renderPlot({
    
    # plot all the class level trends with point for whether increasing or decreasing
    class_SAI %>%
      filter(taxa %in% c("Magnoliopsida", "Liliopsida", "Pinopsida", "Cycadopsida", "Polypodiopsida", "Gnetopsida")) %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = taxa), alpha = 0.3) +
      geom_line(aes(x = Year, y = LPI, colour = taxa), size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
      scale_colour_manual("Taxonomic class", na.translate = F, values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_fill_manual("Taxonomic class", na.translate = F, values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_y_continuous(breaks = c(0.8, 1, 1.2), labels = c("0.8", "1", "1.2")) +
      facet_wrap(~taxa) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # plot of total production vulnerability for each country
  output$class_language_SAI <-  renderPlot({
    
    # plot all the class level trends
    class_language %>%
      filter(taxa %in% c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles")) %>%
      mutate(language = factor(language, levels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish"),
                               labels = c("Ar", "Ch", "En", "Fr", "Ge", "It", "Ja", "Po", "Ru", "Sp"))) %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = taxa), alpha = 0.4) +
      geom_line(aes(x = Year, y = LPI, colour = taxa)) +
      geom_hline(yintercept = 1, linetype = "88", size = 0.3, alpha = 0.3) +
      scale_fill_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_y_continuous(breaks = c(0, 0.5, 1, 1.5), labels = function(x) ifelse(x == 0, "0", x)) +
      facet_grid(language~taxa) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            legend.position = "none",
            text = element_text(size = 16),
            strip.text.y = element_text(angle = 45))
    
  })
  
  # plot of total production vulnerability for each country
  output$class_language_SAI_plants <-  renderPlot({
    
    # plot all the class level trends
    class_language %>%
      filter(taxa %in% c("Magnoliopsida", "Liliopsida", "Pinopsida", "Cycadopsida", "Polypodiopsida", "Gnetopsida")) %>%
      mutate(language = factor(language, levels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish"),
                               labels = c("Ar", "Ch", "En", "Fr", "Ge", "It", "Ja", "Po", "Ru", "Sp"))) %>%
      ggplot() +
      geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = taxa), alpha = 0.4) +
      geom_line(aes(x = Year, y = LPI, colour = taxa)) +
      geom_hline(yintercept = 1, linetype = "88", size = 0.3, alpha = 0.3) +
      scale_fill_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      scale_y_continuous(breaks = c(0, 0.5, 1, 1.5), labels = function(x) ifelse(x == 0, "0", x)) +
      facet_grid(language~taxa) +
      ylab("") +
      xlab(NULL) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), 
            legend.position = "none",
            text = element_text(size = 16),
            strip.text.y = element_text(angle = 45))
    
  })
  
  
  # plot of total production vulnerability for each country
  output$class_language_change <-  renderPlot({
    
    # add labels for factors, sort by predicted value for language and class, and then plot
    class_language_change %>%
      filter(taxa %in% c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles")) %>%
      ggplot() + 
      geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
      geom_errorbar(aes(x = 1, colour = taxa, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se))), position=position_dodge(width = 0.5), width = 0.2) +
      geom_point(aes(x = 1, colour = taxa, y = predicted_values), position=position_dodge(width=0.5)) +
      ylab("Monthly change in SAI") +
      facet_wrap(~language) +
      scale_y_continuous(breaks = c(-0.015, -0.01, -0.005, 0, 0.005), labels = c("-0.015", "-0.010", "-0.005", "0", "0.005")) +
      scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # plot of total production vulnerability for each country
  output$class_language_change_plants <-  renderPlot({
    
    # add labels for factors, sort by predicted value for language and class, and then plot
    class_language_change %>%
      filter(taxa %in% c("Magnoliopsida", "Liliopsida", "Pinopsida", "Cycadopsida", "Polypodiopsida", "Gnetopsida")) %>%
      ggplot() + 
      geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
      geom_errorbar(aes(x = 1, colour = taxa, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se))), position=position_dodge(width = 0.5), width = 0.2) +
      geom_point(aes(x = 1, colour = taxa, y = predicted_values), position=position_dodge(width=0.5)) +
      ylab("Monthly change in SAI") +
      facet_wrap(~language) +
      scale_y_continuous(breaks = c(-0.015, -0.01, -0.005, 0, 0.005), labels = c("-0.015", "-0.010", "-0.005", "0", "0.005")) +
      scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(), legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  # RC 20220411
  # class by language trend for download
  output$class_language_dl <- downloadHandler(
    filename = function(){
      paste("class_language_SAI_", gsub("-", "_", Sys.Date()), ".csv", sep = "")
    },
    content = function(file){
      write.csv(class_language, file, row.names = F)
    }
  )
  
  # class by language change for download
  output$class_language_change_dl <- downloadHandler(
    filename = function(){
      paste("class_language_change_", gsub("-", "_", Sys.Date()), ".csv", sep = "")
    },
    content = function(file){
      write.csv(class_language_change, file, row.names = F)
    }
  )
  
  # class trend for download
  output$class_SAI_dl <- downloadHandler(
    filename = function(){
      paste("class_SAI_", gsub("-", "_", Sys.Date()), ".csv", sep = "")
    },
    content = function(file){
      write.csv(class_SAI, file, row.names = F)
    }
  )
  
  # overall trend for download
  output$overall_SAI_dl <- downloadHandler(
    filename = function(){
      paste("overall_SAI_", gsub("-", "_", Sys.Date()), ".csv", sep = "")
    },
    content = function(file){
      write.csv(overall_SAI, file, row.names = F)
    }
  )
}