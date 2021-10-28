# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

ui <- shinyUI(navbarPage(title=div(tags$a(href="",img(src="zsl_logo.png"), "")), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "The Species Awareness Index",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                     ".navbar-right {
                     float: right !important;
                     }",
                     "body {padding-top: 75px;}"),
                   tabPanel("HOME", value = "home",
                            shinyjs::useShinyjs(),
                            tags$head(
                              tags$script(HTML("
                                         var fakeClick = function(tabName) {
                                         var dropdownList = document.getElementsByTagName('a');
                                         for (var i = 0; i < dropdownList.length; i++) {
                                         var link = dropdownList[i];
                                         if(link.getAttribute('data-value') == tabName) {
                                         link.click();
                                         };
                                         }
                                         };
                                         ")),
                              tags$script(" $(document).ready(function () {
                                              $('#navBar a').bind('click', function (e) {
                                                $(document).scrollTop(0);
                                              });
                                            });"
                              )
                            ),
                            
                            fluidRow(
                              HTML("
                                 <section class='banner'>
                                 <div class='parallax'><br/>
                                 Charles J Sharpk</div>
                                 </section>
                                 ")
                            ),
                            fluidRow(
                              column(3),
                              column(6,
                                     HTML("<center><h1>THE SPECIES AWARENESS INDEX</h1></center>"),
                                     HTML("<center><h5>Measuring our changing awareness of global biodiversity</h5></center>")
                              ),
                              column(3)
                            ),
                            
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # REport cover
                            fluidRow(style="text-align: center; margin: auto; width: 60%;", 
                                     img(src="overall_SAI.png", width="600px", align = "center"),
                                     HTML("<h5><center>Our overall awareness of biodiversity is marginally increasing, although
                                                 there are differences among taxonomic classes and languages, see "), tags$a("trends", onclick="fakeClick('TRENDS')"), HTML("</center></h5>"),
                                     p(),
                                     HTML("<h6><center>The global trend represents "), tags$strong("41,197"), HTML(" IUCN species (amphibians, birds, 
                                                 insects, mammals, ray-finned fishes, and reptiles) across "), tags$strong("10 Wikipedia 
                                                 languages"), HTML(" (Arabic, Chinese, English, French, German, Italian, Japanese, 
                                                 Portuguese, Russian, Spanish) and over "), tags$strong("4 billion page-views"), HTML(".</center></h6>")
                            ),
                            
                            fluidRow(
                              
                              style = "height:50px;"),
                            
                            fluidRow(
                              column(4),
                              column(2,
                                     tags$div(align = "center", 
                                              tags$a("Understand the data", 
                                                     onclick="fakeClick('DATA')", 
                                                     class="btn btn-primary btn-lg")
                                     )
                              ),
                              column(2,
                                     tags$div(align = "center", 
                                              tags$a("Trends by Taxonomic group", 
                                                     onclick="fakeClick('TRENDS')", 
                                                     class="btn btn-primary btn-lg")
                                     )
                              ),
                              column(4)
                            ),
                            fluidRow(
                              
                              style = "height:50px;")
                            
                            
                            
                   ), # Closes the first tabPanel called "Home"
                   tabPanel("TRENDS", value = "TRENDS",
                      fluidRow(
                        column(6,
                               plotOutput("overall_SAI"),
                               h6("The overall species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, German, Italian, Japanese, Portuguese, Russian, and Spanish (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                        column(6,
                               plotOutput("class_SAI"),
                               h6("The species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, German, Italian, Japanese, Portuguese, Russian, and Spanish separated by taxonomic by class (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles)."))),
                      fluidRow(
                        column(6,
                               plotOutput("class_language_SAI"),
                               h6("The species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages for July 2015–March 2020 (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                        column(6,
                               plotOutput("class_language_change"),
                               h6("Average monthly rate of change for the species page species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages (error bars, predicted values of a linear model, fitting average monthly change in the species page SAI as a function of taxonomic class, Wikipedia language, and their interaction). Fitted values are from the linear model with the R function predict (points), and 95% CIs are from the fitted values ± 1.96 multiplied by the SE."))
                      )),
                   tabPanel("ABOUT", value = "about",
                            
                            fluidRow(
                              column(2),
                              column(8,
                                     # Panel for Background on team
                                     div(class="panel panel-default",
                                         div(class="panel-body",  
                                             tags$div( align = "center",
                                                       div( align = "center", 
                                                            h5("About the Data")
                                                       )
                                             ),
                                             tags$p(h6("The Species Awareness Index..."))
                                         )
                                     )
                              ),
                              column(2)
                            ),
                            fluidRow(
                              column(2),
                              column(8,
                                     # Panel for Background on team
                                     div(class="panel panel-default",
                                         div(class="panel-body",  
                                             tags$div( align = "center",
                                                       div( align = "center", 
                                                            h5("About the team")
                                                       )
                                             ),
                                             tags$p(h6("The Species Awareness Index was concieved and created by Joe Millard, Robin Freeman, Richard Gregory, Kate,..")),
                                             
                                             fluidRow(
                                               
                                               # Rob
                                               column(2,
                                                      div(class="panel panel-default", 
                                                          div(class="panel-body",   style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "Robin_Freeman.png", 
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Robin Freeman"),
                                                                tags$h6( tags$i("Head of Indicators & Assessments Research Unit"))
                                                              )
                                                          )
                                                      )
                                               ),
                                               
                                               # Joe
                                               column(2,
                                                      div(class="panel panel-default",
                                                          div(class="panel-body",    style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "Robin_Freeman.png", align = "center",
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Joe Millard"),
                                                                tags$h6( tags$i("PhD Research Student"))
                                                              )
                                                          )
                                                      )
                                               ),
                                               column(3)
                                               
                                             )
                                         )
                                     ) # Closes div panel
                              ), # Closes column
                              column(2)
                            ),
                            
                            fluidRow(
                              column(2),
                              column(8,
                                     # Panel for Background on Data
                                     div(class="panel panel-default",
                                         div(class="panel-body",  
                                             tags$div( align = "center",
                                                       div( align = "center", 
                                                            h5("Acknowledgements")
                                                       )
                                             ),
                                             tags$p(h6("We are very grateful to the following individuals and organisations who have worked with us and/or shared their data.")),
                                             tags$p(h6("Yan Wong, James Rosindell, ?" ))
                                         ) # Closes div panel
                                     ), # Closes column
                                     column(2)
                              )
                            )
                            
                   )  # Closes About tab
                   
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  class_language <- readRDS(here::here("outputs/shiny_outputs/class_language.rds"))
  class_language_change <- readRDS(here::here("outputs/shiny_outputs/class_language_change.rds"))
  class_SAI <- readRDS(here::here("outputs/shiny_outputs/class_trend.rds"))
  overall_SAI <- readRDS(here::here("outputs/shiny_outputs/overall.rds"))
  
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
  output$class_SAI <-  renderPlot({
  
    # plot all the class level trends with point for whether increasing or decreasing
    class_SAI %>%
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

}

# Run the application 
shinyApp(ui = ui, server = server)
  