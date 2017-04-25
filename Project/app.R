#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(Edulevel)
library(ggplot2)
library(DT)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   #1.  Application title
   titlePanel("Educational level for each county"),
   
   #2. Choose percent or count
   radioButtons("type", label = h3("Choose type"),
                choices = list("Percent" = "percent", "Count" = "count"), 
                selected = "percent"),
   
   hr(),
   fluidRow(column(2, verbatimTextOutput("value_type"))),
   
   
      
   #3. Choose level
   radioButtons("level", label = h3("Choose educational level"),
                choices = list("1 Less than a high school diploma"=1, "2 High school diploma only"=2, 
                               "3 Some college (1-3 years)"=3, "4 Bachelor's degree or higher"=4), 
                selected = 4),
   hr(),
   fluidRow(column(4, verbatimTextOutput("value_level"))),
       #plot
   mainPanel(
     plotOutput("mapstate")
   ),
   
   #4 Choose state
   textInput("state", label = h3("Input state name (2 letter)"), value = "IA"),
   
   hr(),
   fluidRow(column(3, verbatimTextOutput("value_state"))),
   #show county
   checkboxInput("countyname", label = "Show county name", value = FALSE),
   mainPanel(h5(textOutput("county"))),
   
   hr(),
   fluidRow(column(12, verbatimTextOutput("value_countyname"))),
   
   #choose limit in mapcounty
   fluidRow(
     column(4,
            
            # Copy the line below to make a slider bar 
            sliderInput("limit", label = h3("Labeled county greater than the limit %"), min = 0, 
                        max = 100, value = 30)
     )),
   hr(),
   fluidRow(
       column(4, verbatimTextOutput("value_limit"))
     ),
      #plot
   mainPanel(
     plotOutput("mapcounty")
   ),
   
   #4 lineEd
   textInput("county", label = h3("Input county name"), value = "Story County"),
   
   hr(),
   fluidRow(column(3, verbatimTextOutput("value_county"))),
   #plot
   mainPanel(
     plotOutput("lineEd")
   ),
   
   #5 Data Table
   mainPanel(h3("Data table for educational level")),
   
   # Create a new Row in the UI for selectInputs
   fluidRow(
     column(2,
            selectInput("t_state",
                        "State:",
                        c("All",
                          unique(as.character(tidy$State))))
     ),
     column(2,
            selectInput("t_area",
                        "Area:",
                        c("All",
                          unique(as.character(tidy$Area))))
     ),
     column(3,
            selectInput("t_level",
                        "Education level:",
                        c("All",
                          unique(as.character(tidy$level))))
     ),
     column(2,
            selectInput("t_type",
                        "Type:",
                        c("All",
                          unique(as.character(tidy$type))))
     ),
     column(2,
            selectInput("t_year",
                        "Year:",
                        c("All",
                          unique(as.character(tidy$year))))
     )
   ),
   # Create a new row for the table.
   fluidRow(
     DT::dataTableOutput("table")
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  #1. output value
  output$value_type <- renderPrint({input$type})
  output$value_level <- renderPrint({input$level})
  output$value_state <- renderPrint({input$state})
  output$value_countyname <- renderPrint({
    if (input$countyname == TRUE)
      showcounty(input$state)
    else
      input$countyname})
  output$value_limit <- renderPrint({input$limit})
  output$value_county <- renderPrint({input$county})
  #2. US map
  output$mapstate <- renderPlot({
    mapstate(vtype=input$type,levelint = input$level)
  })
  
  #3.county map
  output$mapcounty <- renderPlot({
    mapcounty(levelint = input$level,stateshort = input$state, limit=input$limit)
  })
  
  #4 County name
  
  #5 County line: lineEd
  output$lineEd <- renderPlot({
    lineEd(input$state, input$county)
  })
  
  #6 Data table
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Edulevel::tidy
    if (input$t_state != "All") {
      data <- data[data$State == input$t_state,]
    }
    if (input$t_area != "All") {
      data <- data[data$Area == input$t_area,]
    }
    if (input$t_level != "All") {
      data <- data[data$level == input$t_level,]
    }
    if (input$t_type != "All") {
      data <- data[data$type == input$t_type,]
    }
    if (input$t_year != "All") {
      data <- data[data$year == input$t_year,]
    }
    data
  }))
  
}


# Run the application 
shinyApp(ui = ui, server = server)

