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
                                                            h5("About the Index")
                                                       )
                                             ),
                                             tags$p(h6(style="text-align: justify;", "The Species Awareness Index is a global metric of public biodiversity awareness, derived from the rate of change in page views for ~40,000 animal species on Wikipedia, across the 10 most popular Wikpedia languages (Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish).
                                                             Each species page is adjusted for the background change in popularity of the Wikipedia language in which it appears, meaning we can be more confident that change in awareness is not predicted by some background process on Wikipedia."))
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
                                             div(style="text-align: justify", HTML("<p align='justify'> <h6>The Species Awareness Index was originally conceived and created by Joe Millard, Robin Freeman, Richard Gregory, and Kate Jones (see the original publication <a href = 'https://doi.org/10.1111/cobi.13701'> here</a>).</h6></p>")),
                                             
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
                                                                tags$h6(tags$i("Postdoctoral researcher, Leverhulme Centre for Demographic Science"))
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
                                             div(style="text-align: justify", HTML("<h6>We are very grateful to the following individuals and organisations who have worked with us and/or kindly shared their data:
                                                             The Wikimedia Foundation, Yan Wong & James Rosindell (<a href = 'https://www.onezoom.org/'>Onezoom</a>), Tim Newbold, Dave Redding, and the UCL CBER Data Club.</h6></p>")),
                                             
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
                                     h6("The overall species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                              column(6,
                                     plotOutput("class_SAI"),
                                     h6("The species awareness index (SAI) for reptiles, ray-finned fishes, mammals, birds, insects, and amphibians on the Wikipedia languages Arabic, Chinese, English, German, Italian, Japanese, Portuguese, Russian, and Spanish separated by taxonomic by class (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles)."))),
                            br(),
                            fluidRow(
                              column(6,
                                     plotOutput("class_language_SAI"),
                                     h6("The species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages for July 2015–March 2020 (lines, mean of bootstrapped indices at each monthly time step; shading, 2.5th and 97.5th percentiles).")),
                              column(6,
                                     plotOutput("class_language_change"),
                                     h6("Average monthly rate of change for the species page species awareness index (SAI) for 6 taxonomic classes across 10 Wikipedia languages (error bars, predicted values of a linear model, fitting average monthly change in the species page SAI as a function of taxonomic class, Wikipedia language, and their interaction). Fitted values are from the linear model with the R function predict (points), and 95% CIs are from the fitted values ± 1.96 multiplied by the SE."))
                            )), # Closes About tab,
                   tabPanel("DATA", value = "DATA",
                            tabsetPanel(
                              tabPanel("What is the SAI?", 
                                       fluidRow(style="text-align: center; margin: auto; width: 80%;",
                                                br(),
                                                p(style="text-align: justify;", 
                                                  "The Species Awareness Index is a global metric of public biodiversity awareness, derived from the rate of change in page views for ~40,000 animal species on Wikipedia, across the 10 most popular Wikpedia languages (Arabic, Chinese, English, French, German, Italian, Japanese, Portuguese, Russian, and Spanish). Each species page is adjusted for the background change in popularity of the Wikipedia language in which it appears, meaning we can be more confident that change in awareness is not predicted by some background process on Wikipedia."),        
                                       )
                              ),
                              
                              tabPanel("How is it calculated?", 
                                       fluidRow(style="text-align: center; margin: auto; width: 80%;",
                                                br(),
                                                p(style="text-align: justify;", 
                                                  "The Species Awareness Index is calculated from the rate of change of species pages on Wikipedia, using the methdology applied in the Living Planet Index"),        
                                       )
                              )
                            )
                   ),
                   ### RC 20220411
                   
                   # tabPanel("DOWNLOAD", value = "DOWNLOAD",
                   #          fluidRow(
                   #            column(2),
                   #            column(8,
                   #                   # Panel for Background on team
                   #                   # Panel for Background on team
                   #                   div(class="panel panel-default",
                   #                       div(class="panel-body",  
                   #                           h5("Download the data behind the Species Awareness Index"),
                   #                           tags$p(h6("The Species Awareness Index contains trends for tens of thousands of species.")),
                   #                           tags$p(h6("This dataset contains time-series of page-view data for species between 2015 and 2020. Each species 
                   #                              is represented by page-view trends in up to 10 languages, controlled for overall change in page-views 
                   #                              for that wikipedia language ")), 
                   #                           tags$p(h6("Please tick the box below to agree to our data-use agreement: "), tags$a(href="data_agreement_2020.pdf", "data_agreement_2020.pdf")),
                   #                           checkboxInput("download_check",
                   #                                         label = "Agree"
                   #                           ),
                   #                           downloadButton("download_alldata", "Download")
                   #                       )
                   #                   )
                   #            ),
                   #            column(2)
                   #          )
                   # )  # Closes the second tabPanel called "Download"
                   
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