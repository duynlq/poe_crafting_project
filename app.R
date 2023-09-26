library(shiny)

ui <- fluidPage(
  titlePanel("Duy's POE Crafter"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
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
  Roll <- 1:19 # Updated to 19 rolls
  Weight <- c(
    # # to maximum Life ----
    rep((1000/(9-3+1)), (9-3+1)),
    rep((1000/(19-10+1)), (19-10+1)),
    rep((1000/(29-20+1)), (29-20+1)),
    rep((1000/(39-30+1)), (39-30+1)),
    rep((1000/(49-40+1)), (49-40+1)),
    rep((1000/(59-50+1)), (59-50+1)),
    rep((1000/(69-60+1)), (69-60+1)),
    rep((1000/(79-70+1)), (79-70+1)),
    rep((1000/(89-80+1)), (89-80+1)),
    rep((1000/(99-90+1)), (99-90+1)),
    rep((1000/(109-100+1)), (109-100+1)),
    rep((1000/(119-110+1)), (119-110+1)),
    rep((1000/(129-120+1)), (129-120+1)),
    # #% increased Armour ----
    rep((1000/(26-15+1)), (26-15+1)),
    rep((1000/(42-27+1)), (42-27+1)),
    rep((1000/(55-43+1)), (55-43+1)),
    rep((1000/(67-56+1)), (67-56+1)),
    rep((1000/(79-68+1)), (79-68+1)),
    rep((1000/(91-80+1)), (91-80+1)),
    rep((1000/(100-92+1)), (100-92+1)),
    rep((1000/(110-101+1)), (110-101+1)),
    # #% increased Armour, #% increased Stun and Block Recovery ----
    rep((1000/(13-6+1)), (13-6+1)),
    rep((1000/(20-14+1)), (20-14+1)),
    rep((1000/(26-21+1)), (26-21+1)),
    rep((1000/(32-27+1)), (32-27+1)),
    rep((1000/(38-33+1)), (38-33+1)),
    rep((1000/(42-39+1)), (42-39+1)),
    # # to Armour ----
    rep((1000/(12-6+1)), (12-6+1)),
    rep((1000/(35-13+1)), (35-13+1)),
    rep((1000/(63-36+1)), (63-36+1)),
    rep((1000/(82-64+1)), (82-64+1)),
    rep((1000/(101-83+1)), (101-83+1)),
    rep((1000/(120-102+1)), (120-102+1)),
    rep((1000/(150-121+1)), (150-121+1)),
    rep((1000/(200-151+1)), (200-151+1)),
    rep((1000/(300-201+1)), (300-201+1)),
    rep((1000/(400-301+1)), (400-301+1)),
    rep((1000/(500-401+1)), (500-401+1)),
    # to Armour, # to maximum Life ----
    rep((1000/(12-6+1)), (12-6+1)),
    rep((1000/(35-13+1)), (35-13+1)),
    rep((1000/(63-36+1)), (63-36+1)),
    rep((1000/(82-64+1)), (82-64+1)),
    
  ) # End Weights ----
  
  Prob <- Weight / sum(Weight)
  Cumulative <- cumsum(Prob)
  
  counter <- reactiveVal(0)
  
  # Generate custom roll names dynamically
  custom_roll_names <- c()
  # # to maximum Life ----
  for (i in 3:9) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 10:19) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 20:29) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 30:39) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 40:49) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 50:59) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 60:69) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 70:79) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 80:89) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 90:99) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 100:109) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 110:119) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  for (i in 120:129) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to maximum Life")) }
  # #% increased Armour ----
  for (i in 15:26) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 27:42) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 43:55) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 56:67) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 68:79) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 80:91) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 92:100) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  for (i in 101:110) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour")) }
  # #% increased Armour, #% increased Stun and Block Recovery ----
  for (i in 6:13) {
    if (i < ((13-6)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "6% increased Stun and Block Recovery")) }
    if (i > ((13-6)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "7% increased Stun and Block Recovery")) }
  }
  for (i in 14:20) {
    if (i < ((20-14)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "8% increased Stun and Block Recovery")) }
    if (i > ((20-14)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "9% increased Stun and Block Recovery")) }
  }
  for (i in 21:26) {
    if (i < ((26-21)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "10% increased Stun and Block Recovery")) }
    if (i > ((26-21)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "11% increased Stun and Block Recovery")) }
  }
  for (i in 27:32) {
    if (i < ((32-27)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "12% increased Stun and Block Recovery")) }
    if (i > ((32-27)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "13% increased Stun and Block Recovery")) }
  }
  for (i in 33:38) {
    if (i < ((38-33)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "14% increased Stun and Block Recovery")) }
    if (i > ((38-33)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "15% increased Stun and Block Recovery")) }
  }
  for (i in 42:39) {
    if (i < ((42-39)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "16% increased Stun and Block Recovery")) }
    if (i > ((42-39)/2)) { custom_roll_names <- c(custom_roll_names, paste0(i, " % increased Armour, ", "17% increased Stun and Block Recovery")) }
  }
  
  
  
  
  # # to Armour ----
  for (i in 6:12) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 13:35) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 36:63) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 64:82) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 83:101) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 102:120) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 121:150) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 151:200) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 201:300) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 301:400) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  for (i in 401:500) { custom_roll_names <- c(custom_roll_names, paste0("+", i, " to Armour")) }
  
  
  
  # End custom_roll_names ----
  
  custom_roll_names <- c(custom_roll_names#,
                         #"#% Increased Armour",
                         #"#% Increased Armour, #% Increased Stun and Block Recovery",
                         #"#% To Armour",
                         #"#% To Armour, To Maximum Life",
                         #"Reflects # Physical Damage To Melee Attackers"
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
    HTML(paste("<p style='clear:both;'>Alts Used:", counter(), "<br>", 
               #headers, "<br>", paste(rolls_info, collapse = "<br>"), 
               "</p>"))
  })
}

shinyApp(ui, server)