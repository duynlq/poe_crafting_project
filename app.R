library(shiny)

ui <- fluidPage(
  titlePanel("Duy's POE Crafter"),
  sidebarLayout(
    sidebarPanel(
      width=4,
      actionButton("roll_button", 
                   label = tags$img(src = "https://playerverse.com/wp-content/uploads/2021/08/orb-of-alteration.png", 
                                    width = 50, height = 50)),
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
  
  counter <- reactiveVal(0)
  
  # Define custom roll names
  custom_roll_names <- c(
    "# to Maximum Life",
    "#% Increased Armour",
    "#% Increased Armour, #% Increased Stun and Block Recovery",
    "#% To Armour",
    "#% To Armour, To Maximum Life",
    "Reflects # Physical Damage To Melee Attackers"
  )
  
  rolls_history <- c() # Store the history of rolls
  
  observeEvent(input$roll_button, {
    rand_num <- runif(1)
    selected_roll_index <- which(Cumulative >= rand_num)[1]
    selected_roll_name <- custom_roll_names[selected_roll_index]
    
    rolls_history <<- c(rolls_history, selected_roll_name) # Append to the history
    
    # Increment the counter value when the button is clicked
    current_value <- counter()
    updated_value <- current_value + 1
    counter(updated_value)
    
    output$result <- renderText({
      paste("", selected_roll_name)
    })
    
    output$roll_history <- renderPrint({
      cat("History:\n", paste(rev(rolls_history), collapse = "\n"))
    })
  })
  
  output$roll_info <- renderUI({
    headers <- "<b>Prefix</b><span style='float:right;'><b>Weight</b></span>"
    rolls_info <- paste(custom_roll_names, "<span style='float:right;'>", Weight, "</span>")
    HTML(paste("<p style='clear:both;'>You Used:", counter(), "Alts<br>", headers, "<br>", paste(rolls_info, collapse = "<br>"), "</p>"))
  })
}

shinyApp(ui, server)
