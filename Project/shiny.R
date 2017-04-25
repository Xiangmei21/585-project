library(shiny)
require(ggplot2)
require(dplyr)
library(plotly)
library(Edulevel)

Edulevel::tidy
state.list <- as.list(state.abb)
names(state.list) <- state.name

ui <- navbarPage(
  titlePanel(h3("Educational level in the US")),
  fluidRow(),
    
    tabPanel(h3("Plot"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("state", "State: ", choices = state.list),
                 uiOutput("showCounty"),
                 radioButtons("level","Education level", 
                              choices =list("Less than a high school diploma"=1, "High school diploma only"=2, 
                                            "Some college (1-3 years)"=3, "Bachelor's degree or higher"=4)),
                 sliderInput("limit","Labeled county greater than the limit %", 
                             min = 0, max = 100, value = 30)
               ),
               mainPanel(
                 plotlyOutput("plotstate"),
                 plotlyOutput("plotcounty"),
                 plotlyOutput("linecounty")
                 )
             )),
    tabPanel(
             mainPanel(h3("Data")),
             fluidRow(),
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(2,
                      selectInput("t_state",
                                  "State:",
                                  c("All",
                                    unique(as.character(tidy$State))))
               ),
               column(3,
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
    
  
)


server <- function(input, output, session) {
  output$showCounty <- renderUI({
    p.county = showcounty(input$state)
    selectInput("county", "County: ", choices = p.county)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(tidy, options = list(orderClasses = TRUE))
  })
  
  output$plotstate <- renderPlotly({
    gg1 <- mapstate(vtype="percent",levelint=input$level)
    print(ggplotly(gg1))
  })
  output$plotcounty <- renderPlotly({
    gg2 <- mapcounty(vtype="percent",levelint=input$level, stateshort = input$state, limit=input$limit)
    print(ggplotly(gg2))
  })
  
  output$linecounty <- renderPlotly({
    gg2 <- lineEd(stateshort = input$state, countyname = input$county)
    print(ggplotly(gg2))
  })
  
}


# Bind ui and server together
shinyApp(ui, server)
