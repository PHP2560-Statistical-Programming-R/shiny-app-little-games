library(shiny)

ui <- fluidPage(
  headerPanel("P-value Calculator"),
  br(),
  hr(),
  sidebarLayout(
    sidebarPanel(
          numericInput(inputId = "z", label = "Z-value", value = 0, step = 0.01),
          selectInput("hypothesis", "One-tailed or two-tailed hypothesis?",
                      choices = c("One-tailed hypothesis",
                                  "Two-tailed hypothesis")),
          actionButton(inputId = "calc", label = "Calculate")),
    mainPanel(
      verbatimTextOutput("p")
    )
  )
)


server <- function(input, output){
  
  output$p = renderText({
    Type <- input$hypothesis
    if (Type == "One-tailed hypothesis"){
    input$calc
    isolate(paste("P-value is ", pnorm(input$z, lower.tail = FALSE), sep = ""))

  } else if (Type == "Two-tailed hypothesis"){
    input$calc
    isolate(paste("P-value is ", 2*pnorm(input$z, lower.tail = FALSE), sep = ""))
  }
  })
  
}

shinyApp(ui = ui, server = server)