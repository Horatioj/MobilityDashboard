library(shiny)
library(shinycssloaders)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(geojsonR)
library(shinyjs)
library(readxl)
library(igraph)
library(rintrojs)

ui <- fluidPage(
    actionButton(inputId="help", label="Help", icon=icon("question"), style = "position: absolute; top: 5px; right: 20px; z-index:9999;"),
    introjsUI(),
    useShinyjs(),
    navbarPage("Mobility Equity Map", id = "nav",
               # first tab, a gif
               tabPanel(title = introBox("Intro", data.step=5, data.intro = "Switch to introduction page to review MI & MEM concepts."),
                        div(class="outer"),
                        # tags$head(
                        #   includeCSS("styles.css"),
                        # ),
                        img(src = "Intro.gif", width = "100%", height = "100%", type = "image/gif", autoplay = TRUE, loop = FALSE),
               ),
               
               # second tab, two leafletOutputs with height 50% each
               tabPanel(title=introBox("DC", data.step=6, data.intro="Choropleth and isochrone maps in Washington, D.C."),
                        div(class="outer",
                            tags$head(
                              includeCSS("styles.css")
                            ),
                            leafletOutput("map", width="100%", height="50%"),
                            
                            # border line
                            tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                            # select Mobility, Population, Income and a histogram
                            # need to click the submit button below
                            fixedPanel(
                              id = "controls", class = "panel panel-default", draggable = TRUE, top = 60, left = "auto",
                              right = 50, bottom = "auto", width = 500, height = "auto",
                              fluidRow(
                                column(6, style = "margin-top: 20px;",
                                       introBox(
                                  actionButton("ok", "Press for instructions"),
                                  data.intro = "Press to start instructions", data.step = 12, data.position = "auto"
                                )),
                                column(6, introBox(
                                  selectInput("Columns", "", choices = c("MobilityIndex", "Population", "Income"), selected = "MobilityIndex"),
                                  data.step = 1,
                                  data.intro = "Select different variables to visualize the choropleth map on the left. Note: panels are draggable."
                                )),
                                # column(12, introBox(
                                #   plotOutput("Histogram", height = 350),
                                #   data.step = 2,
                                #   data.intro = "The chart shows a positive relationship between mobility index and population."
                                # ))
                              ),
                              # h2("Controller"),
                              # introBox(
                              #   actionButton("ok", "Press for instructions"),
                              #   data.intro = "Press to start instructions", data.step = 5, data.position = "auto",
                              #   width = "100%"
                              # ),
                              # introBox(
                              # selectInput("Columns", "Select", choices = c("Mobility", "Population", "Income"),
                              #             selected = "Mobility", width = "100%"), data.step = 1,
                              # data.intro = "This is the 1st step"),
                              # 
                              # # with spinner, 
                              # # fail to work on leafletoutputs probably due to the shiny doesn't 
                              # # send calculation signs and shinycssloaders cannot detect
                              # withSpinner(
                              #   introBox(
                              #   plotOutput("Histogram", width="100%"), data.step = 3, data.intro = "This is the 3rd step"), type = 7, 
                              # ),s
                              # withSpinner(plotOutput("Scatter", height = 300), type = 7)
                            ),
                            # the boxplot - jitter - smooth line
                            
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 550, left =  "auto", 
                              right = 60, bottom = "auto", width = 500, height = "auto",
                              
                              withSpinner(
                                introBox(
                                plotOutput("Scatter", height = 300), data.step = 3, data.intro = "The chart illustrates a relationship between the MI and community median income. The gray 'x' indicates outliers in the boxplot.", data.position = "auto"
                              ), type = 7)
                            ),
                            # isochrones, can select communities, and 4 time sidebars
                            # click submit button to see changes
                            leafletOutput("isochrone", width="100%", height="50%"),
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = "auto", left = 60, 
                              right = "auto", bottom = 0, width = 350, height = "auto",
                              
                              # h2("Controller"),
                              introBox(fluidRow(
                              selectInput("community", "Departure Point", choices = c("Arboretum, Anacostia River", "Capitol Hill, Lincoln Park", "Cathedral Heights, McLean Gardens, Glover Park",
                                                                                "Downtown, Chinatown, Penn Quarters, Mount Vernon Square, North Capitol Street",
                                                                                "Dupont Circle, Connecticut Avenue K Street", "Edgewood, Bloomingdale, Truxton Circle, Eckington",
                                                                                "Georgetown, Burleith Hillandale", "Near Southeast, Navy Yard",
                                                                                "Observatory Circle", "Union Station, Stanton Park, Kingman Park")),
                              # selectInput("transport", "Transportation", choices = c("Bicycle", "Drive", "Transit", "Walk")),
                              
                                sliderInput("slider1", "Bicycle Time [min]", 10, 60, 10, 10),
                                sliderInput("slider2", "Drive Time [min]", 10, 60, 10, 10),
                                sliderInput("slider3", "Transit Time [min]", 10, 60, 10, 10),
                                sliderInput("slider4", "Walk Time [min]", 10, 60, 10, 10),
                              ), data.step = 4, data.intro = "You can change the isochrones by changing the departure point and the time in minutes for the different modes of travel. The MEM on the right will be changed as a result."),

                              # actionButton("submit", "Submit", icon("refresh"), class = "btn btn-primary", width = "100%")
                        ))
               ),
               
               # third tab, two leafletOutputs with height 50% each
               tabPanel(title=introBox("NYC", data.step=7, data.intro="Choropleth and isochrone maps in the New York City"),
                        div(class="outer",
                            tags$head(
                              includeCSS("styles.css")
                            ),
                            leafletOutput("nymaps", width="100%", height="50%"),
                            # border line
                            tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                            # select Mobility, Population, Income and a histogram
                            # need to click the submit button below
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                              right = 20, bottom = "auto", width = 450, height = "auto",
                              
                              selectInput("nyColumns", "Select", choices = c("MobilityIndex", "Population", "Income"),
                                          selected = "MobilityIndex"),
                              
                              # withSpinner(
                              #   plotOutput("nyHist", height = 350), type = 7, 
                              # ),
                            ),
                            # the boxplot - jitter - smooth line
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 550, left =  "auto", 
                              right = 60, bottom = "auto", width = 500, height = "auto",
                              withSpinner(
                                plotOutput("nyScat", height = 300), type = 7
                              )
                            ),
                            leafletOutput("nyiso", width="100%", height="50%"), 
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = "auto", left = 60, 
                              right = "auto", bottom = 0, width = 350, height = "auto",
                              
                              selectInput("nycommunity", "Departure Point", choices = c("Midtown", "John F. Kennedy International Airport", "Long Island City", 
                                                                                  "Ozone Park", "Washington Heights")),
                              sliderInput("nyslider1", "Bicycle Time [min]", 10, 60, 10, 10),
                              sliderInput("nyslider2", "Drive Time [min]", 10, 60, 10, 10),
                              sliderInput("nyslider3", "Transit Time [min]", 10, 60, 10, 10),
                              sliderInput("nyslider4", "Walk Time [min]", 10, 60, 10, 10),
                              # submitButton("Submit", width = '100%')
                            )
                            
                        )
                        
               ),
              # fourth tab, two leafletOutputs with height 50% each
               tabPanel(title=introBox("Chicago", data.step=8, data.intro = "Choropleth and isochrone maps in Chicago"),
                        div(class="outer",
                            tags$head(
                              includeCSS("styles.css")
                            ),
                            leafletOutput("mapch", width="100%", height="50%"),
                            # border line
                            tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                            absolutePanel(
                              id = "controls", class = "panel panel-default",
                              fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                              right = 20, bottom = "auto", width = 450, height = "auto",
                              
                              selectInput("chColumns", "Select", choices = c("MobilityIndex", "Population", "Income"),
                                          selected = "MobilityIndex"),
                            ),
                            # the boxplot - jitter - smooth line
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 550, left =  "auto", 
                              right = 60, bottom = "auto", width = 500, height = "auto",
                              
                              withSpinner(
                                plotOutput("Scatch", height = 300), type = 7
                              )
                            ),
                            leafletOutput("isoch", width="100%", height="50%"), 
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = "auto", left = 60, 
                              right = "auto", bottom = 0, width = 350, height = "auto",
                              
                              selectInput("chcommunity", "Departure Point", choices = c("Bridgeport", "Edgewater", "Gold Coast",
                                                                                 "Greektown", "Jackson Park")),
                              sliderInput("chslider1", "Bicycle Time [min]", 10, 60, 10, 10),
                              sliderInput("chslider2", "Drive Time [min]", 10, 60, 10, 10),
                              sliderInput("chslider3", "Transit Time [min]", 10, 60, 10, 10),
                              sliderInput("chslider4", "Walk Time [min]", 10, 60, 10, 10),
                            )
                        )       
               ),
               # fifth tab, two leafletOutputs with height 50% each
               tabPanel(title=introBox("LA", data.step=9, data.intro = "Choropleth and isochrone maps in Los Angeles"),
                        div(class="outer",
                            tags$head(
                              includeCSS("styles.css")
                            ),
                            leafletOutput("mapla", width="100%", height="50%"),
                            # border line
                            tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                              right = 20, bottom = "auto", width = 450, height = "auto",
                              
                              selectInput("laColumns", "Select", choices = c("MobilityIndex", "Population", "Income"),
                                          selected = "MobilityIndex"),
                            ),
                            # the boxplot - jitter - smooth line
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 550, left =  "auto", 
                              right = 60, bottom = "auto", width = 500, height = "auto",
                              
                              withSpinner(
                                plotOutput("Scatla", height = 300), type = 7
                              )
                            ),
                            leafletOutput("isola", width="100%", height="50%"), 
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = "auto", left = 60, 
                              right = "auto", bottom = 0, width = 350, height = "auto",
                              
                              selectInput("lacommunity", "Departure Point", choices = c("Century City", "Del Rey", "Downtown",
                                                                                 "Hollywood", "Northridge")),
                              sliderInput("laslider1", "Bicycle Time [min]", 10, 60, 10, 10),
                              sliderInput("laslider2", "Drive Time [min]", 10, 60, 10, 10),
                              sliderInput("laslider3", "Transit Time [min]", 10, 60, 10, 10),
                              sliderInput("laslider4", "Walk Time [min]", 10, 60, 10, 10),
                            )
                        )       
               ),
               # sixth tab, two leafletOutputs with height 50% each
               tabPanel(title=introBox("Boston", data.step=10, data.intro = "Choropleth and isochrone maps in Boston"),
                        div(class="outer",
                            tags$head(
                              includeCSS("styles.css")
                            ),
                            leafletOutput("mapbs", width="100%", height="50%"),
                            # border line
                            tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                            # select Mobility, Population, Income and a histogram
                            # need to click the submit button below
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                              right = 20, bottom = "auto", width = 450, height = "auto",
                              
                              selectInput("bColumns", "Select", choices = c("MobilityIndex", "Population", "Income"),
                                          selected = "MobilityIndex"),
                            ),
                            # the boxplot - jitter - smooth line
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = 550, left =  "auto", 
                              right = 60, bottom = "auto", width = 500, height = "auto",
                              
                              withSpinner(
                                plotOutput("Scatbs", height = 300), type = 7
                              )
                            ),
                            leafletOutput("isobs", width="100%", height="50%"), 
                            absolutePanel(
                              id = "controls", class = "panel panel-default", 
                              fixed = TRUE, draggable = TRUE, top = "auto", left = 60, 
                              right = "auto", bottom = 0, width = 350, height = "auto",
                              
                              selectInput("bcommunity", "Departure Point", choices = c("Allston", "Back Bay", "Charlestown",
                                                                                 "South Boston", "West End")),
                              sliderInput("bslider1", "Bicycle Time [min]", 10, 60, 10, 10),
                              sliderInput("bslider2", "Drive Time [min]", 10, 60, 10, 10),
                              sliderInput("bslider3", "Transit Time [min]", 10, 60, 10, 10),
                              sliderInput("bslider4", "Walk Time [min]", 10, 60, 10, 10),
                            )
                            
                        )
                        
               ),
               # seventh tab, network
               tabPanel(title=introBox("TravelNet", data.step = 11, data.intro = "Switch to transportation flow simulation in Boston. 
                                       You can toggle public transit rate, weight on public transit, and non-compliance rates to find travel time (in seconds) of routes and transportation network's MEM."),
                        div(class="outer"),
                        tags$head(
                          includeCSS("styles.css"),
                          tags$style(HTML("
      # #myPlot {
      # background-color: transparent !important;
      #   position: absolute;
      #   width: 80%;
      #   height: 80%;
      #   top: 25%;
      #   left: 5%;
      #   z-index: 1;
      # }
  
      #basemap {
        position: absolute;
        width: 100%;
        height: 100%;
        top: 5%;
        left: 0;
        z-index: 0;
      }
      
       /* Custom notification styles */
      .shiny-notification {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background-color: #f8f9fa; /* Light gray background */
        border: 1px solid #ced4da; /* Light border */
        border-radius: 5px; /* Rounded corners */
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* Shadow effect */
        padding: 15px 20px; /* Padding around the content */
        font-family: Arial, sans-serif; /* Font family */
        font-size: 16px; /* Font size */
        color: #333; /* Text color */
      }
      .shiny-notification .close {
        position: absolute;
        top: 5px;
        right: 10px;
        font-size: 16px;
        color: #888;
        cursor: pointer;
      }
      .shiny-notification .close:hover {
        color: #555;
      }
    "))
                        ),
                        
                        leafletOutput("basemap", width = "100%", height = "100%"),
                        # withSpinner(
                        #   imageOutput("Network"), type = 7,
                        # ),
                        # withSpinner(
                        #   imageOutput("myPlot", width = "70%", height = "400px"), type = 7
                        # ),
                        # img(src = "file.png", width = "100%", height = "100%", type = "image/png"),
                        absolutePanel(
                          id = "controls", class = "panel panel-default", 
                          fixed = TRUE, draggable = TRUE, top = 80, left =  200, 
                          right = "auto", bottom = "auto", width = 300, height = "auto",
                          selectInput("P", "Public Transit Rate", choices = c(0.3, 0.5, 0.7)),
                          selectInput("W", "Weight on Public", choices = c(0.6, 0.7, 0.8)),
                          selectInput("N", "Non-Compliance Rate", choices = c(0.1, 0.3, 0.5)),
                          # submitButton("Submit", width = '100%')
                        ),
                        
               ),
               
    ))