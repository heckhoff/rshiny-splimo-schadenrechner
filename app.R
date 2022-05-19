library(shiny)
library(shinyjs)
library(shinyBS)
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
      
      numericInput(
        "speed",
        "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
        min = 1,
        value = 0
      ),
      
      br(),
      textOutput("weapon"),
      br(),
      textOutput("mean_dmg"),
      br(),
      textOutput("mean_dmg_per_tick"),
      br(),
      textOutput("sd_dmg"),
      br(),
      
      h4("Waffenmerkmale:"),
      sliderInput(
        "exact",
        "Exakt",
        min = 0,
        max = 5,
        value = 0
      ),
      bsTooltip(id = "exact", title = "Bei einem Schadenswurf mit dieser Waffe werden so viele Würfel zusätzlich geworfen, wie die Stufe des Merkmals beträgt. Die höchsten Ergebnissezählen für den Schadenswurf.", 
                trigger = "hover"),
      sliderInput(
        "sharp",
        "Scharf",
        min = 0,
        max = 5,
        value = 0
      ),
      bsTooltip(id = "sharp", title = "Alle Schadenswürfel einer Waffe mit diesem Merkmal werden immer als mindestens der Wert der Stufe des Merkmals gewertet, egal was eigentlich gewürfelt wurde.",
                trigger = "hover"),
      sliderInput(
        "critical",
        "Kritisch",
        min = 0,
        max = 5,
        value = 0
      ),
      bsTooltip(id = "critical", title = "Der Schaden eines Angriffs einer Waffe mit diesem Merkmal erhöht sich für jeden Schadenswürfel, der die maximale Augenzahl würfelt, um die Stufe des Merkmals.", 
                trigger = "hover"),
      br(),
      
      selectInput(
        "y_axis",
        "Darstellung der kumulierten Grafik",
        choices = list(
          "minimaler Schaden" = "cum_prob_min",
          "maximaler Schaden" = "cum_prob_max"
        )
      ),
      br(),
      actionButton("reset_input", "Eingabe zurücksetzen"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("dist_plot"),
              br(),
              plotOutput("cum_dist_plot"))
  )
)
# Backend ----
# Define server logic required to draw a histogram
server <- function(input, output) {
  prob_table <- reactive(
    create_prob_table(
      n_d6 = input$d6,
      n_d10 = input$d10,
      flat_mod = input$flat,
      att_exact = input$exact,
      att_sharp = input$sharp,
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
    reactive(paste("Durchschn. Schaden:",
                   round(prob_table()[, sum(damage * probability)], 2)))
  output$mean_dmg <- renderText({
    mean_damage()
  })
  
  # Print Average Damage per Tick
  mean_damage_per_tick <-
    reactive(paste(
      "Durchschn. Schaden pro Tick:",
      fifelse(input$speed != 0, yes =
      round(prob_table()[, sum(damage * probability)] / input$speed, 2),
            no = 0)
    ))
  output$mean_dmg_per_tick <- renderText({
    mean_damage_per_tick()
  })
  
  # Print Damage Standard Deviation
  sd_damage <-
    reactive(paste("Durchschnittliche Abweichung:",
                   round(prob_table()
                         [, sqrt(sum((damage - sum(
                           damage * probability
                         ))
                         ^ 2 * probability))], 2)))
  output$sd_dmg <- renderText({
    sd_damage()
  })
  
  # Plot Probability Distribution
  output$dist_plot <- renderPlot({
    x <- prob_table()
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = probability)) +
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
  })
  
  # Plot Cumulative Probability Distribution
  output$cum_dist_plot <- renderPlot({
    x <- prob_table()
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = switch(
      input$y_axis,
      cum_prob_min = cum_prob_min,
      cum_prob_max = cum_prob_max
    ))) +
      geom_bar(stat = "identity",
               color = "blueviolet",
               fill = "dodgerblue1") +
      ggtitle(switch(
        input$y_axis,
        cum_prob_min = "Kumulierte Wahrscheinlichkeiten (Mindestschaden)",
        cum_prob_max = "Kumulierte Wahrscheinlichkeiten (Maximalschaden)"
      )) +
      xlab("Schaden") +
      ylab("Wahrscheinlichkeit in %") +
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_y_continuous(
        labels = function(x)
          paste0(x * 100, "%"),
        breaks = pretty_breaks(n = 10)
      ) +
      theme_classic(base_size = 20)
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
