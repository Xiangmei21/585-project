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
                 radioButtons("type", label = h3("Choose a type:"),
                              choices = list("Percent" = "percent", "Count" = "count"), 
                              selected = "percent"),
                 selectInput("state", h3("Choose a State:"), choices = state.list, selectize=TRUE),
                 uiOutput("showCounty"),
                 radioButtons("level",h3("Education level:"), 
                              choices =list("Less than a high school diploma"=1, "High school diploma only"=2, 
                                            "Some college (1-3 years)"=3, "Bachelor's degree or higher"=4),
                              selected =4)
                 #sliderInput("limit",h3("List areas greater than the limit %"), 
                 #             min = 0, max = 100, value = 30)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Country level",plotlyOutput("plotstate")),
                   tabPanel("State level",plotlyOutput("plotcounty")),
                   tabPanel("County level",plotlyOutput("linecounty"))
                 )
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
    selectInput("county", "County: ", choices = p.county, selectize=TRUE)
  })
  
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
  
  
  output$plotstate <- renderPlotly({
    
    ggplotly(mapstate(vtype=input$type,levelint=input$level))
  })
  output$plotcounty <- renderPlotly({
    gg2 <- mapcounty(vtype=input$type,levelint=input$level, stateshort = input$state)
    print(ggplotly(gg2))
  })
  
  
  output$linecounty <- renderPlotly({
    gg3 <- lineEd(stateshort = input$state, countyname = input$county)
    print(ggplotly(gg3))
  })
  
}


# Bind ui and server together
shinyApp(ui, server)
