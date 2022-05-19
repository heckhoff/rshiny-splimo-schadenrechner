library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(scales)

source("damage_calculation.R")

# Frontend ----
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "flatly",
  useShinyjs(),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  # Application title
  titlePanel("Splittermond Waffenschadenrechner"),
  # navbarMenu(
  # Sidebar with a slider input for number of bins
  ## Side-Panel ----
  sidebarLayout(
    sidebarPanel(
      id = "side-panel",
      fluidRow(column(
        6,
        numericInput(
          "d6",
          "Anzahl W6 Würfel",
          min = 0,
          max = 8,
          value = 0
        )
      ),
      column(
        6,
        numericInput(
          "d10",
          "Anzahl W10 Würfel",
          min = 0,
          max = 5,
          value = 0
        )
      )),
      numericInput("flat",
                   "Modifikator",
                   value = 0),
      
      numericInput(
        "speed",
        "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
        min = 1,
        value = 0
      ),
      
      ## Textoutput ----
      br(),
      textOutput("weapon"),
      # br(),
      textOutput("mean_dmg"),
      # br(),
      textOutput("mean_dmg_per_tick"),
      # br(),
      textOutput("sd_dmg"),
      br(),
      
      ## Waffenmerkmale ----
      h4("Waffenmerkmale:"),
      fluidRow(
        column(
          6,
          sliderInput(
            "exact",
            "Exakt",
            min = 0,
            max = 5,
            value = 0
          ),
          bsTooltip(
            id = "exact",
            title = "Bei einem Schadenswurf mit dieser Waffe werden so viele Würfel zusätzlich geworfen, wie die Stufe des Merkmals beträgt. Die höchsten Ergebnisse zählen für den Schadenswurf.",
            trigger = "hover"
          ),
          sliderInput(
            "critical",
            "Kritisch",
            min = 0,
            max = 5,
            value = 0
          ),
          bsTooltip(
            id = "critical",
            title = "Der Schaden eines Angriffs einer Waffe mit diesem Merkmal erhöht sich für jeden Schadenswürfel, der die maximale Augenzahl würfelt, um die Stufe des Merkmals.",
            trigger = "hover"
          )
        ),
        column(
          6,
          sliderInput(
            "penetration",
            "Durchdringung",
            min = 0,
            max = 6,
            value = 0
          ),
          bsTooltip(
            id = "penetration",
            title = "Für jede Stufe dieses Merkmals kann 1 Punkt der gegnerischen Schadensreduktion ignoriert werden, egal aus welcher Quelle diese stammt.",
            trigger = "hover"
          ),
          sliderInput(
            "sharp",
            "Scharf",
            min = 0,
            max = 5,
            value = 0
          ),
          bsTooltip(
            id = "sharp",
            title = "Alle Schadenswürfel einer Waffe mit diesem Merkmal werden immer als mindestens der Wert der Stufe des Merkmals gewertet, egal was eigentlich gewürfelt wurde.",
            trigger = "hover"
          )
        )
      ),
      br(),
      # Schadensreduktion ----
      h4("Simulierte Schadensreduktion:"),
      sliderInput(
        "dmg_reduction",
        "SR",
        min = 0,
        max = 10,
        value = 0
      ),
      bsTooltip(
        id = "dmg_reduction",
        title = "Die Schadensreduktion einer Rüstung wird von dem Schaden jedes erfolgreichen Angriffs gegen den Träger abgezogen.",
        trigger = "hover"),
      
      br(),
      actionButton("reset_input", "Eingabe zurücksetzen"),
    ),
    
    ## Main-Panel ----
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel(
        "Exakte Wahrscheinlichkeiten",
        plotOutput("dist_plot", width = "85%", height = "300px"),
        br(),
        selectInput(
          "y_axis",
          "Darstellung der kumulierten Grafik",
          choices = list(
            "minimaler Schaden" = "cum_prob_min",
            "maximaler Schaden" = "cum_prob_max"
          )
        ),
        plotOutput("cum_dist_plot", width = "85%", height = "300px")
      ),
      tabPanel("Schadensreduktion")
    ))
  )
)
# Backend ----
# Define server logic required to draw a histogram
server <- function(input, output) {
  # observe({
  #   if (all(c(input$d6, input$d10) == 0)) {
  prob_table <- reactive({
    req(any(c(input$d6, input$d10) != 0))
    create_prob_table(
      n_d6 = input$d6,
      n_d10 = input$d10,
      flat_mod = input$flat,
      att_exact = input$exact,
      att_sharp = input$sharp,
      att_critical = input$critical,
      att_penetration = input$penetration,
      damage_reduction = input$dmg_reduction
    )
  })
  #   }
  # })
  # Textoutput ----
  # Print Selected Weapon
  print_weapon_txt <-
    reactive(paste(
      "Ausgewählte Waffe:",
      create_weapon_txt(input$d6, input$d10, input$flat)
    ))
  output$weapon <- renderText({
    print_weapon_txt()
  })
  
  # Calculate Average Damage
  mean_damage <- reactive(round(prob_table()[, sum(damage * probability)], 2))
  # Print Average Damage
  print_mean_damage <-
    reactive(paste("Durchschn. Schaden:",
                   mean_damage()))
  output$mean_dmg <- renderText({
    print_mean_damage()
  })
  
  # Print Average Damage per Tick
  print_mean_damage_per_tick <-
    reactive(paste(
      "Durchschn. Schaden pro Tick:",
      fifelse(
        input$speed != 0,
        yes =
          round(prob_table()[, sum(damage * probability)] / input$speed, 2),
        no = 0
      )
    ))
  output$mean_dmg_per_tick <- renderText({
    print_mean_damage_per_tick()
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
  
  # theme_old <- theme_bw()
  # theme_new <- theme_set(theme_bw())
  # theme_new <- theme_update(panel.grid.major.x = element_blank())
  
  # Plots ----
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
    }
    )
  
  observe({
    if (all(c(input$d6, input$d10) == 0)) {
      hide("weapon")
      hide("mean_dmg")
      hide("dist_plot")
      hide("y_axis")
    } else {
      show("weapon")
      show("mean_dmg")
      show("dist_plot")
      show("y_axis")
    }
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
