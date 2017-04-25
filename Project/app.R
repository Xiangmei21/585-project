#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Edulevel)

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
   checkboxInput("countyname", label = "Show county name", value = TRUE),
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
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  #1. output value
  output$value_type <- renderPrint({input$type})
  output$value_level <- renderPrint({input$level})
  output$value_state <- renderPrint({input$state})
  output$value_countyname <- renderPrint({showcounty(input$state)})
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
  
  #5 County line
  output$lineEd <- renderPlot({
    lineEd(input$state, input$county)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

