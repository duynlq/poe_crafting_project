library(shiny)

ui <- fluidPage(
  titlePanel("POE Crafting Simulation"),
  sidebarLayout(
    sidebarPanel(
      actionButton("roll_button", "Use Orb of Alteration"),
      uiOutput("roll_info")
    ),
    mainPanel(
      textOutput("result"),
      verbatimTextOutput("roll_history")
    )
  )
)

server <- function(input, output) {
  Roll <- 1:6
  Weight <- c(13000, 8000, 6000, 11000, 3500, 4000)
  Prob <- c(0.2857, 0.1758, 0.1319, 0.2418, 0.0769, 0.0879)
  Cumulative <- cumsum(Prob)
  
  rolls_history <- c() # Store the history of rolls
  
  observeEvent(input$roll_button, {
    rand_num <- runif(1)
    selected_roll <- Roll[which(Cumulative >= rand_num)[1]]
    
    rolls_history <<- c(rolls_history, selected_roll) # Append to the history
    
    output$result <- renderText({
      paste("You rolled a", selected_roll)
    })
    
    output$roll_history <- renderPrint({
      cat("Roll history:\n", paste(rev(rolls_history), collapse = "\n"))
    })
  })
  
  output$roll_info <- renderUI({
    rolls_info <- paste("Roll", Roll, "- Weight:", Weight)
    HTML(paste("<p>", paste(rolls_info, collapse = "<br>"), "</p>"))
  })
}

shinyApp(ui, server)
