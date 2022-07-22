# library in aws package for read in of data
library(aws.s3)

# each of these csv reads needd to be replaced by a call to AWS, eventually to SQL database
s3BucketName <- "speciesawarenessindex"

# read in each of the secret keys hosted online
AWS_ACCESS_KEY_ID <- read.table("AWS_ACCESS_KEY_ID.txt")
AWS_SECRET_ACCESS_KEY <- read.table("AWS_SECRET_ACCESS_KEY.txt")
AWS_DEFAULT_REGION <- read.table("AWS_DEFAULT_REGION.txt")

# set system environment for each of AWS keys
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION)

# read in each of the rds files from AWS
overall_SAI <- s3read_using(FUN = readRDS, bucket = s3BucketName, object = "overall_2.rds")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

shinyUI(navbarPage(title=div(tags$a(href="",img(src="zsl_logo.png"), "")), id = "navBar",
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
                              style = "height:50px;")),
                   tabPanel("ABOUT", value = "about",
                            fluidRow(
                              column(2),
                              column(8,
                                     # Panel for Background on team
                                     div(class="panel panel-default",
                                         div(class="panel-body",  
                                             tags$div( align = "center",
                                                       div( align = "center", 
                                                            h5("About this website")
                                                       )
                                             ),
                                             tags$p(h6(style="text-align: justify;", "This website hosts a real-time version of the Species Awareness Index (v2.0), a global metric of public biodiversity awareness, derived from the rate of change in page views for ~40,000 animal species on Wikipedia, across the 10 most popular Wikpedia languages (Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish).")),
                                                       
                                             tags$p(h6(style="text-align: justify;", "SAI v2.0 updates automatically each month, with the most recent data underpinning the index made available for download at any given time here. In developing a real-time version of the SAI, the core pipeline has now undergone a full rebuild, thereby transitioning it away from the rLPI R package on which it was originally built (all code is available on GitHub here). V2.0 demonstrates how conservation culturomics metrics represent an ideal candidate for the development of real-time biodiversity monitoring platforms (see our preprint here).")),
                                                       
                                             tags$p(h6(style="text-align: justify;", "Our hope is that through an online and open portal of changing species awareness, we can help facilitate research to better understand interest in biodiversity online."))
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
                                             tags$div(align = "center",
                                                      div(align = "center", 
                                                          h5("About the team")
                                                      )
                                             ),
                                             div(style="text-align: justify", HTML("<p align='justify'> <h6>The Species Awareness Index was originally conceived and created by Joe Millard, Robin Freeman, Richard Gregory, and Kate Jones (see the original publication <a href = 'https://doi.org/10.1111/cobi.13701'> here</a>). The real-time version hosted on this website (v2.0) was built by Joe Millard, Richard Cornford, Thomas Frederick-Johnson, Shawn Dove, and Robin Freeman.</h6></p>")),
                                             
                                             fluidRow(
                                               # Rob
                                               column(2,
                                                      div(class="panel panel-default", 
                                                          div(class="panel-body",   style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "Robin_Freeman.jpg", 
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
                                                                tags$img(src = "joe_millard.jpg", align = "center",
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Joe Millard"),
                                                                tags$h6(tags$i("Postdoctoral researcher, the Natural History Museum"))
                                                              )
                                                          )
                                                      )
                                               ),
                                               # Shawn
                                               column(2,
                                                      div(class="panel panel-default",
                                                          div(class="panel-body",    style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "shawn_dove.jfif", align = "center",
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Shawn Dove"),
                                                                tags$h6(tags$i("PhD student, IoZ & UCL"))
                                                              )
                                                          )
                                                      )),
                                               column(2,
                                                      div(class="panel panel-default",
                                                          div(class="panel-body",    style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "tom_johnson.jpg", align = "center",
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Thomas Frederick-Johnson"),
                                                                tags$h6(tags$i("Postdoctoral researcher, Sheffield University"))
                                                              )
                                                          )
                                                      )),
                                               column(2,
                                                      div(class="panel panel-default",
                                                          div(class="panel-body",    style = "height:300px", 
                                                              align = "center",
                                                              div(
                                                                tags$img(src = "rich_placeholder.jpg", align = "center",
                                                                         width = "90px", height = "90px")
                                                              ),
                                                              div(
                                                                tags$h5("Richard Cornford"),
                                                                tags$h6(tags$i("PhD student, IoZ & UCL"))
                                                              )
                                                          )
                                                      ))
                                             ))))),
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
                                             div(style="text-align: justify", HTML("<h6>We are very grateful to the following individuals and organisations who have worked with us and/or kindly shared their data: The Wikimedia Foundation for making Wikipedia view data available via APIs; Yan Wong & James Rosindell (<a href = 'https://www.onezoom.org/'>Onezoom</a>) for supplying the taxonomic reference data that underpins the linking of pages among Wikipedia languages; and a number of researchers who have helped us out along the way including Tim Newbold, Dave Redding, and the UCL CBER Data Club. Thanks also to the RPSB and NERC who funded the initial development of the SAI.
                                                             </h6></p>")),
                                             
                                         ) # Closes div panel
                                     ), # Closes column
                                     column(2)
                              )
                            )
                   ), 
                   tabPanel("TRENDS", value = "TRENDS",
                            fluidRow(
                              column(6,
                                     plotOutput("overall_SAI"),
                                     h6(style = "text-align: justify;", paste("Figure 1. The overall species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish, for the period July 2015-", format(as.Date(tail(overall_SAI$Year, 1)), "%B %Y"), sep = ""), "(lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                              
                              column(6,
                                     plotOutput("class_SAI"),
                                     h6(style = "text-align: justify;", paste("Figure 2. The species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, German, Italian, Japanese, Portuguese, Russian, and Spanish separated by taxonomic by class, for the period July 2015-", format(as.Date(tail(overall_SAI$Year, 1)), "%B %Y"), sep = ""), "(lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles)."))),
                            br(),
                            fluidRow(
                              column(6,
                                     plotOutput("class_language_SAI"),
                                     h6(style = "text-align: justify;", paste("Figure 3. The species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages for July 2015-", format(as.Date(tail(overall_SAI$Year, 1)), "%B %Y"), sep = ""), "(lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                              column(6,
                                     plotOutput("class_language_change"),
                                     h6(style = "text-align: justify;", paste("Figure 4. Average monthly rate of change for the species page species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages, for the period January 2016-", format(as.Date(tail(overall_SAI$Year[grepl("-01-", overall_SAI$Year)], 1)), "%B %Y"), sep = ""), "(error bars, predicted values of a linear model, fitting average monthly change in the species page SAI as a function of taxonomic class, Wikipedia language, and their interaction). Fitted values are from the linear model with the R function predict (points), and 95% CIs are from the fitted values ± 1.96 multiplied by the SE."))
                            )), # Closes About tab,
                   tabPanel("DATA", value = "DATA",
                            tabsetPanel(
                              tabPanel("What is the SAI?", 
                                       fluidRow(style="text-align: center; margin: auto; width: 80%;",
                                                br(),
                                                p(style="text-align: justify;", "The Species Awareness Index (SAI) is a global metric of public biodiversity awareness, derived from the rate of change in page views for ~40,000 animal species on Wikipedia, across the 10 most popular Wikpedia languages (Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish). Each species page is adjusted for the background change in popularity of the Wikipedia language in which it appears, meaning we can be more confident that change in awareness is not predicted by some background process on Wikipedia."),
                                                p(style="text-align: justify;", "The SAI tells us, in real-time, how interest in animal biodiversity is changing. For example, as the seasons change we can track how this relates to interest in biodiversity, with peaks in interest tracking the phenological emergence of species. We can also better understand how significant world events such as the COVID-19 pandemic affected interest in biodiversity, or how viral videos on particular animal species relate to changes in interest online."),
                                                p(style="text-align: justify;", "As of July 2022, the SAI incorporates xxx views in total, collected over a period of xxx years and xxx months from xxx species pages and xxx random pages."))
                                       ),
                              
                              
                              tabPanel("How is it calculated?", 
                                       fluidRow(style="text-align: center; margin: auto; width: 80%;",
                                                br(),
                                                p(style="text-align: justify;", "The SAI is a new measurement of change in species awareness calculated at the species page level from the rate of change in daily average Wikipedia views per month. Because the SAI measures the rate of change in views within a species page, species are weighted equally irrespective of their popularity, meaning highly viewed species do not dominate the SAI. The methodology underpinning the Species Awareness Index was heavily inspired by the Living Planet Index, which tracks changes in vertebrate population size over time."), 
                                                p(style="text-align: justify;", "The original version of the SAI relied heavily on the R package rlpi to calculate an index of change over time. The code still functions in the same way, but has undergone a full rebuild to make it more amenable to running on the cloud. There are four core steps to the generation of the SAI, besides downloading views from the Wikipedia API: 1) build a species level index of change; 2) adjust change in that species level trend for the background change in Wikipedia; 3) smooth the random adjusted index of change; and 4) average change for a single species across language; 5) bootstrap the smoothed random adjusted change for groups of species (e.g. mammals or birds). Below we provide a short summary of each key step (for more details please see the original SAI paper here)."), 
                                                p(style="text-align: justify;", strong("1) Build a species level index of change for each species page")), 
                                                p(style="text-align: justify;", "For each species in 6 taxonomic groups (amphibians, birds, insects, mammals, ray-finned fish, and reptiles) on 10 Wikipedia languages (Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish), we apply a generalized additive model to smooth the daily average species page view trends. These smoothed values are then used to calculate a rate of change in views for each species page article."), 
                                                p(style="text-align: justify;", strong("2) Adjust change in each species page trend for the background change in Wikipedia")), 
                                                p(style="text-align: justify;", "To account for the overall change in popularity of Wikipedia itself over the same period, we adjusted the rate of change for each species page with the rate of change in a random set of complete series Wikipedia pages. For each species page, this adjustment was made with a random set of pages in the Wikipedia language of that species page. To do so, we first calculated the rate of change for each random page in each language, as in species pages. We then used a bootstrap resampling approach to calculate the average rate of change for all random pages in a given language at each time step. The average rate of change in the random pages was calculated by bootstrapping the monthly rates of change 1000 times and then extracting the bootstrapped mean. At each time step, we then adjusted the species page rate of change by subtracting the monthly bootstrap estimated random rate of change."), 
                                                p(style="text-align: justify;", strong("3) Smooth the random adjusted index of change for each species page")), 
                                                p(style="text-align: justify;", "To account for differences in the tortuosity of trends among Wikipedia languages, we smoothed the species page SAI in each Wikipedia language with a loess regression (span = 0.3) before transforming the smoothed species page SAI back into a rate of change."), 
                                                p(style="text-align: justify;", strong("4) Average change for a single species across languages")), 
                                                p(style="text-align: justify;", "After smoothing the species page SAI as above, we calculated a species SAI for each species (across languages) by averaging rates of change at each time step across all languages."), 
                                                p(style="text-align: justify;", strong("5) Bootstrap change in groups of species")), 
                                                p(style="text-align: justify;", "We then calculated an overall SAI combining all species across 10 Wikipedia languages by averaging rates of change across all species SAIs. Bootstrap confidence intervals were calculated by taking the 2.5th and 97.5th percentiles of 1000 bootstrapped indices at each time step. Using the same approach as above, we calculated an overall SAI for each taxonomic class for all languages combined and for each taxonomic class in each language (albeit each species page here is not averaged across languages)."))
                                       )
                              )
                            ),
                   #),
                   ### RC 20220411
                   ## UI code to create tab panel with download buttons per data frame
                   tabPanel("DOWNLOAD", value = "DOWNLOAD",
                            fluidRow(
                              column(2),
                              column(8,
                                     # Panel for downloads
                                     div(class="panel panel-default",
                                         div(class="panel-body",  
                                             h5("Download the data behind the Species Awareness Index"),
                                             tags$p(h6("The Species Awareness Index contains trends for tens of thousands of species.")),
                                             tags$p(h6("This dataset contains time-series of page-view data for species between 2015 and 2020. Each species 
                                                         is represented by page-view trends in up to 10 languages, controlled for overall change in page-views 
                                                         for that wikipedia language ")), 
                                             tags$p(h6("Please tick the box below to agree to our data-use agreement and access the data: "), 
                                                    tags$a(href="data_agreement_2020.pdf", "data_agreement_2020.pdf")),
                                             checkboxInput("download_check",
                                                           label = "Agree"
                                             ),
                                             br(),
                                             ##
                                             conditionalPanel(condition = "input.download_check ==  1 ", # unquoted 1
                                                              tags$p(h6("Click on the button below to download the data behind the overall SAI.")),
                                                              downloadButton("overall_SAI_dl", label = "Download 1"),
                                                              
                                                              br(),
                                                              
                                                              tags$p(h6("To download the data behind the class-level SAI click below.")),
                                                              downloadButton("class_SAI_dl", label = "Download 2"),
                                                              
                                                              br(),
                                                              
                                                              tags$p(h6("To download data for SAIs per class and language, use the button below.")),
                                                              downloadButton("class_language_dl", label = "Download 3"),
                                                              
                                                              br(),
                                                              
                                                              tags$p(h6("The averge monthly change in SAI per class and language cn be downloaded here.")),
                                                              downloadButton("class_language_change_dl", label = "Download 4")
                                             )
                                             
                                         )
                                     )
                              ),
                              column(2)
                            )
                   )  # Closes the second tabPanel called "Download"
                   
                   ###
)
)