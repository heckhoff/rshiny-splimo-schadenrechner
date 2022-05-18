library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)

source("damage_calculation.R")

# Frontend ----
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  # Application title
  titlePanel("Splittermond Waffenschadenrechner"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      id = "side-panel",
      
      numericInput(
        "d6",
        "Anzahl W6 Würfel",
        min = 0,
        max = 8,
        value = 0
      ),
      numericInput(
        "d10",
        "Anzahl W10 Würfel",
        min = 0,
        max = 5,
        value = 0
      ),
      numericInput("flat",
                   "Modifikator",
                   value = 0),
      br(),
      
      h4("Waffenmerkmale:"),
      sliderInput(
        "exact",
        "Exakt",
        min = 0,
        max = 5,
        value = 0
      ),
      sliderInput(
        "critical",
        "Kritisch",
        min = 0,
        max = 5,
        value = 0
      ),
      br(),
      
      selectInput(
        "y_axis",
        "Darstellung",
        choices = list(
          "exakte Wahrscheinlichkeit" = "prob",
          "minimaler Schaden (kumuliert)" = "cum_prob_min",
          "maximaler Schaden (kumuliert)" = "cum_prob_max"
        )
      ),
      textOutput("weapon"),
      br(),
      textOutput("mean_dmg"),
      br(),
      actionButton("reset_input", "Eingabe zurücksetzen")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("dist_plot"))
  )
)
# Backend ----
# Define server logic required to draw a histogram
server <- function(input, output) {
  prob_table <- reactive(create_prob_table(
    n_6s = input$d6,
    n_10s = input$d10,
    flat_mod = input$flat,
    att_exact = input$exact,
    att_critical = input$critical
  )
  )
  # Print Selected Weapon
  weapon_txt <-
    reactive(paste(
      "Ausgewählte Waffe:",
      create_weapon_txt(input$d6, input$d10, input$flat)
    ))
  output$weapon <- renderText({
    weapon_txt()
  })
  
  # Print Average Damage
  mean_damage <-
    reactive(paste(
      "Durchschnittlicher Schaden:",
      round(prob_table()[, sum(damage * probability)], 2)
    ))
  output$mean_dmg <- renderText({
    mean_damage()
  })
  
  # Plot Probability Distribution
  output$dist_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- prob_table()
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = switch(
      input$y_axis,
      prob = probability,
      cum_prob_min = cum_prob_min,
      cum_prob_max = cum_prob_max
    ))) +
      geom_bar(stat = "identity",
               color = "blueviolet",
               fill = "dodgerblue1") +
      ggtitle("Wahrscheinlichkeitsverteilung des Schadens") +
      xlab("Schaden") +
      ylab("Wahrscheinlichkeit in %") +
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_y_continuous(
        labels = function(x)
          paste0(x * 100, "%"),
        breaks = pretty_breaks(n = 10)
      ) +
      theme_classic(base_size = 20)
    # draw the histogram with the specified number of bins
    # hist(x, breaks = nrows(x), col = 'darkgray', border = 'white')
  })
  
  observe({
    if (all(c(input$d6, input$d10) == 0)) {
      hide("weapon")
      hide("mean_dmg")
      hide("dist_plot")
    } else {
      show("weapon")
      show("mean_dmg")
      show("dist_plot")
    }
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
