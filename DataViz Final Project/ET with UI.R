library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(dplyr)
library(ggplot2)


ui <- dashboardPage(
    dashboardHeader(title = "Airfare"),
    dashboardSidebar(
        sidebarMenu(
                       menuItem("Data", tabName = "page4", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page4",
                    selectInput(inputId = "Departure_City", label = "Where are you flying out from?", "Cities"),
                    DT::dataTableOutput("myTable"),
                    selectInput(inputId = "Destination_City", label = "Where are you flying to?", "Cities"),
                    plotlyOutput("myPlot"))
            )
        )
    )


server <- function(input, output, session) {
    setwd("C:/Users/eunic/OneDrive/Desktop/JHU/Data Visualization/Project")
    data = read_csv("Airfare_Report.csv")
    locations = read_csv("GeoLocation.csv")
    print("start")
    
    #selective input
    observe({
        updateSelectInput(session, "Departure_City", choices = locations$City)
    })

    #select departure city
    ET_departure1 <-  reactive({
        req(input$Departure_City)
        data %>%
        filter(city1 == input$Departure_City) %>%
        rename(Departure = city1) %>%
        rename(Destination = city2)
    })
    

        
    ET_departure2 <-reactive({
        req(input$Departure_City)
        data %>%
        filter(city2 == input$Departure_City) %>%
        rename(Departure = city2) %>%
        rename(Destination = city1) 
    })
    

    ET_departure <- reactive({
        rbind(ET_departure1(), ET_departure2()) 
    })
    
    #return list of overall avg airfare
    ET_total_fare_avg <- reactive({
        ET_departure() %>%
            group_by(Destination) %>%
            summarise(Average_Fare = round(mean(fare), digits =2))
    })

    output$myTable <- DT::renderDataTable({
        DT::datatable(ET_total_fare_avg())        
    })
    
    #categorize by quarter
    ET_Q1 <- reactive({
        req(input$Destination_City)
        ET_departure %>%
        filter(quarter == "1") %>%
        group_by(Destination) %>%
        summarise(Q1_Average_Fare = mean(fare)) 
    ET_Q1$Q1_Average_Fare <- round(ET_Q1$Q1_Average_Fare,2)
    })
    
    ET_Q2 <- reactive({
        req(input$Destination_City)
        ET_departure %>%
        filter(quarter == "2") %>%
        group_by(Destination) %>%
        summarise(Q2_Average_Fare = mean(fare)) 
    ET_Q2$Q2_Average_Fare <- round(ET_Q2$Q2_Average_Fare,2)
    })
    
    ET_Q3 <- reactive({
        req(input$Destination_City)
        ET_departure %>%
        filter(quarter == "3")%>%
        group_by(Destination) %>%
        summarise(Q3_Average_Fare = mean(fare)) 
    ET_Q3$Q3_Average_Fare <- round(ET_Q3$Q3_Average_Fare,2)
    })
    
    ET_Q4 <- reactive({
        req(input$Destination_City)
        ET_departure %>%
        filter(quarter == "4")%>%
        group_by(Destination) %>%
        summarise(Q4_Average_Fare = mean(fare)) 
    ET_Q4$Q4_Average_Fare <- round(ET_Q4$Q4_Average_Fare,2)
    })
    
    ET_First_half <- reactive({full_join(ET_Q1,ET_Q2)})
    ET_Last_half <- reactive({full_join(ET_Q3,ET_Q4)})
    ET_Season_fare <- reactive({full_join(ET_First_half,ET_Last_half)})
    
    SelectET <- reactive({
        req(input$Destination_City)
        ET_Season_fare %>%
        filter(Destination == input$Destination_City)
    })
    
    FareET <- reactive({c(SelectET$Q1_Average_Fare,SelectET$Q2_Average_Fare, SelectET$Q3_Average_Fare, SelectET$Q4_Average_Fare)})
    Quarter <- c('1', '2','3','4')
    ET_df_temp <- reactive({data.frame(Quarter,FareET)})
    ET_df <- reactive({ET_df_temp %>%
        mutate(FareET = replace_na(FareET, mean(FareET, na.rm = TRUE)))
    })
    
    
    output$myPlot <- renderPlot({
        ET_Season_plot <- ET_df %>%
            ggplot(ET_df, aes(x=Quarter, y=FareET, fill=FareET)) +
            geom_bar(stat = "identity") + 
            coord_cartesian(ylim = c(min(FareET)-50, max(FareET)+10)) + 
            scale_fill_gradient(low = "#88D0C7", high = "#D08891")+
            labs(title="Average Fare Trend", y = "Airfare", fill='Average Fare')
    })   

}

shinyApp(ui = ui, server = server)

