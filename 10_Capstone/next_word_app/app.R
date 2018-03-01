library(shiny)
library(shinythemes)
library(markdown)

source("step4_query_DT.R")

ui <- fluidPage(
   theme = shinytheme("flatly"),
   
   titlePanel("Predict Next Word", windowTitle = "Next Word"),
   
   sidebarLayout(
      sidebarPanel(
          
          helpText("Start typing below"),
          textInput(inputId = "usertext",
                     label = "Enter text here:")
      ),
      
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Predictions",
                               h3("Predictions include..."),
                               uiOutput("button1"),
                               br(),
                               uiOutput("button2"),
                               br(),
                               uiOutput("button3")),
                      
                      tabPanel("Documentation",
                               includeMarkdown("about_capstone.Rmd"),
                               br())
                      )
      )
   )
)

# Define server logic required to predict word
server <- function(input, output) {
    
    split_query <- reactive({
        validate(
            need(input$usertext != "", "Please enter text")
        )
        query_call <- query_dt(process_input(input$usertext))
        unlist(strsplit(query_call, " "))
    })

    output$button1 <- renderUI({
        actionButton("action1", label = split_query()[1])
    })
    output$button2 <- renderUI({
        actionButton("action2", label = split_query()[2])
    })
    output$button3 <- renderUI({
        actionButton("action3", label = split_query()[3])
    })
    
    my_clicks <- reactiveValues(data = NULL)
    
    observeEvent(input$action1, {
        my_clicks$data <- split_query()[1]
    })
    
    observeEvent(input$action2, {
        my_clicks$data <- split_query()[2]
    })  
    
    observeEvent(input$action3, {
        my_clicks$data <- split_query()[3]
    })  
    
}

# Run the application 
shinyApp(ui = ui, server = server)