library(DT)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard) 
library(tidyverse)

data<-read_csv("Consumer_Airfare_Report__Table_1a_-_All_U.S._Airport_Pair_Markets.csv")

ui <- dashboardPage(
    dashboardHeader(title = "The Flight Board"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "page1", icon = icon("home")),
            menuItem("Passengers", tabName = "page2", icon = icon("male")),
            menuItem("Airlines", tabName = "page3", icon = icon("plane")),
            menuItem("Airports", tabName = "page4", icon = icon("building")),
            menuItem("Dataset", tabName = "page5", icon = icon("table")),
            menuItem("About Us", tabName = "page6", icon = icon("info-circle"))
            
        )
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #556D8F;
                              color: white;
                              font-family: helvetica;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #556D8F;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #6f90bf;
                              }
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #556D8F;
                              font-family: helvetica;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f5f9ff;
                              color:black;
                              }
                              
                              /* sidebar when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                              color:white;
                              background-color:  #6f90bf;
                              }
                              
                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #f5f9ff;
                              backround-size: cover
                              }'))),
      
        tabItems(
            tabItem(tabName = "page1",
                    img(src="plane.jpg",alt="Plane and Sunset", width='100%', height='100%'),
                    h1("Welcome to The Flight Board", align="center"),
                    "Our team is developing The Flight Board using a dataset from Transportation.gov. The dataset contains 228,000 observations of travel between airport pair markets (departure airport – arrival airport) between 1993 and 2021. We have included the dataset's data elements in the PDF attached to this project plan.",
                    br(),
                    br(),
                    "We are choosing to approach the data set from three stakeholders' perspectives: passengers, airlines, and airports. We believe this reflects real-world use cases that often require the development of multiple visualizations for different stakeholder groups for various purposes from a shared data pool.",
                    br(),
                    br(),
                    "Each of our identified stakeholder groups has different questions or problems for which they seek answers or insights. Our initial areas of focus are listed below."
                    ),
            
            tabItem(tabName = "page2",
                    h1("Passenger Dashboard"),
                    "Using the dataset, we will develop visualizations aimed to provide customer insight on airfare, flight destination, number of passengers, etc., that will help in advanced travel planning and provide a different visual view of flights across the United States.",
                    br(),
                    br(),
                    box(title="Explore Travel Destinations",width=12,
                     "Curious about where you can fly? Designate a starting airport to begin exploring possible destinations. You can also choose additional options including the maximum price you're willing to spend, your preferred airline carriers, and the time of year at which you'd like to travel!",
                      br(),
                      br(),
                      fluidRow(
                        column(2,
                          h4("Select Options"), 
                          img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                           ),
                        column(5,
                           img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                           ),
                        column(5,
                           img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                           )
                        )
                      ),
                    br(),
                    br(),
                    box(title="Explore Direct Flight Destinations",width=12,
                        "Don't like layovers or multiple leg flights? You're not alone! Designate a starting airport below to see all possible destinations with direct flights. You can also choose additional options including your preferred airline carrier and the time of year at which you'd like to travel!",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                          )
                        )
                    )
                    ),
            tabItem(tabName = "page3",
                    h1("Airline Carrier Dashboard"),
                    "The airline industry is a competitive environment, especially in those large metropolitan areas where there are multiple airports customers can choose from. We will explore the fares offered by different airlines at these different airports within the same city market. We will identify which airlines fly to these different airports. And then, we will look at the market share over specific city pairs over the years, see the difference in fares, and deduce passenger retention from the number of passengers. We will explore the myth suggesting airlines charge more in correlation to the distance and determine if the fares are tied to supply and demand instead.",
                    br(),
                    br(),
                    box(title="City Market Dashboard.",width=12,
                        "For a given city market, identify associated airports, airlines, and fares by time of year. Designate a starting city market below to see all possible city market destinations with direct flights. You can also choose additional options including the year and quarter of the data that you'd like to explore",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="barchartplaceholder.png",alt="Bar Chart Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Table Placeholder", width='100%', height='100%')
                          )
                        )
                      ),
                    br(),
                    br(),
                    box(title="City Market Pair Dashboard",width=12,
                        "For a given city market pair, explore if there is a correlation between the distance of the city pairs and average fare.",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          )
                        )
                    ),
                    br(),
                    br(),
                    box(title="Airport Pair Dashboard",width=12,
                        "For a given airport pair, explore the passenger totals per airline, how these have changed over time, and how the airfare rates have changed over time.",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="mapplaceholder.png",alt="Map Placeholder", width='100%', height='100%')
                          )
                        ),
                        fluidRow(
                          column(2),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Table Placeholder", width='100%', height='100%')
                          ),
                          column(5)
                        )
                    )
                  ),
            tabItem(tabName = "page4",
                    h1("Airport Dashboard"),
                    "Airports are designed to optimize both safety and effective operations. Understanding airline flight volumes and passenger counts allow the airport to optimize terminal and gate assignments, staffing requirements, and placement of amenities (e.g., restaurants, restrooms, transportation services). We will develop visualizations that provide insight into flight and passenger volumes. Further, we'd like to compare airports by flight volumes and passenger counts. These elements may be helpful for airports looking to compare services, contracts, or performance with airports of similar service loads.",
                    br(),
                    br(),
                    box(title="Airport Expected Passenger Loads.",width=12,
                        "For a given airport, what passenger load should be expected.",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(10,
                                 img(src="plotplaceholder.png",alt="Plot Placeholder", width='500px', height='200px')
                          )
                        )
                      ),
                    box(title="Airport Expected Passenger Loads Changes Over Time.",width=12,
                        "For a given airport, what are the year-over-year, quarter-over-quarter changes in expected passenger loads?",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(10,
                                 img(src="plotplaceholder.png",alt="Plot Placeholder", width='500px', height='200px')
                          )
                        )
                      ),
                    box(title="Top 10 Airports by Flight and Passenger Volumes",width=12,
                        "For a given time range, what are the top 10 airports by flight and passenger volumes?",                        
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          )
                        )
                    ),
                    box(title="Airports grouped by Flight Volume and Passenger Count",width=12,
                        "For a given time range, what are the top 10 airports by flight and passenger volumes.",
                        br(),
                        br(),
                        fluidRow(
                          column(2,
                                 h4("Select Options"), 
                                 img(src="widgetplaceholder.png",alt="Widget Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          ),
                          column(5,
                                 img(src="imageplaceholder.png",alt="Image Placeholder", width='100%', height='100%')
                          )
                        )
                    )
                  ),
            tabItem(tabName = "page5",
                    h1("Dataset Information"),
                    "Our team developed ‘The Flight Board’ using a dataset from ", a("Transportation.gov",href="https://data.transportation.gov/Aviation/Consumer-Airfare-Report-Table-1a-All-U-S-Airport-P/tfrh-tu9e",target="_blank"),". The dataset contains 228,000 observations of airport pair markets (departure airport – arrival airport) between 1993 and 2021. The data elements in the dataset are listed at the bottom of this page.",
                    br(), 
                    br(), 
                    dataTableOutput("dataTable")
                    ),
            tabItem(tabName = "page6",
                    h1("About Us"),
                    br(),
                    br(),
                    box(title="Team 101",width=12,
                        fluidRow(
                          column(3,
                                 img(src="imageplaceholder.png",alt="Eunice Tseng", width='100px'), align = "center",
                                 h4("Eunice Tseng", align="center"),
                                 h6("ctseng14@jhu.edu", align="center")
                          ),
                          column(3,
                                 img(src="imageplaceholder.png",alt="Patrick King", width='100px'), align="center",
                                 h4("Patrick King", align="center"),
                                 h6("pking19@jhu.edu", align="center")
                          ),
                          column(3,
                                 img(src="imageplaceholder.png",alt="Timothy Sham", width='100px'), align="center",
                                 h4("Timothy Sham", align="center"),
                                 h6("lsham1@jhu.edu", align="center")
                          ),
                          column(3,
                                 img(src="imageplaceholder.png",alt="Todd Andrew", width='100px'), align="center",
                                 h4("Todd Andrew", align="center"),
                                 h6("tandre15@jhu.edu", align="center")
                          )
                        )
                    ),
                    box(title="Acknowledgements",width=12,
                        fluidRow(
                          column(12,
                          "This project was undertaken in the Data Visualization course in the Johns Hopkins Carey Business School's Master of Business Administration (MBA) program. Please ", a("click here", href="https://carey.jhu.edu/programs/mba-programs",target="_blank")," for more information about Johns Hopkins MBR program.",
                          br(),
                          br(),
                          "The team members would like to thank ", a("Dr. Mohammed Ali Alamdar Yazdi", href="https://carey.jhu.edu/faculty/faculty-directory/mohammad-ali-alamdar-yazdi-phd", target="_blank"), " for his teaching and guidance throughout this course. He has significantly increased our skills in data cleaning, transformation, manipuation, and visualization methods using the R programming language. We look forward to continuing to build these skills throughout our careers.",
                          br(), 
                          br(),
                          "Thank you, Dr. Yazdi, for your willingness to share your time and expertise with your students. It is very much appreciated!",
                          )
                        )
                    ),
                )
            )
        )
    )



server <- function(input, output, session) {

    
    output$plot1 = renderPlot({
        
    })
    
    output$plot2 = renderPlotly({
        
    })
    
    output$myMap = renderLeaflet({
        
    })
    
    output$dataTable <- renderDataTable({
      return(datatable(data,rownames=FALSE))
    })

}

shinyApp(ui = ui, server = server)
