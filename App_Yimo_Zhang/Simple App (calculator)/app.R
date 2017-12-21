library(shiny)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  headerPanel("P-value Calculator"),
  br(),
  hr(),
  sidebarLayout(
    sidebarPanel(
      div(id = "cal",
      numericInput(inputId = "z", label = "Z-value", value = 0),
      sliderInput(inputId = "mean", label = "Mean", value = 0, min = -20, max = 20),
      sliderInput(inputId = "variance", label = "Variance", value = 1, min = 1, max = 30),
      actionButton(inputId = "go", label = "Calculate")),
      br(),
      actionButton(inputId = "reset", "Reset")
    ),
    mainPanel(
    verbatimTextOutput("p")
  )
  )
)

server <- function(input, output){
  
  output$p = renderText({
    input$go
    isolate(paste("P-value = ", pnorm(-input$z, mean = input$mean, sd = sqrt(input$variance)), sep = ""))
  })
  
  observeEvent(input$reset, reset("cal"))
}
 
shinyApp(ui = ui, server = server)