library(shiny)
library(shinythemes)
library(shinydashboard)
# library(shinyWidgets)
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
      bsCollapse(
        id = "weapons",
        open = "weapon_1",
        bsCollapsePanel(
          "weapon_1",
          fluidRow(
            column(4,
                   numericInput(
                     "d6",
                     "Anzahl W6",
                     min = 0,
                     max = 8,
                     value = 0
                   )),
            column(4,
                   numericInput(
                     "d10",
                     "Anzahl W10",
                     min = 0,
                     max = 5,
                     value = 0
                   )),
            column(4,
                   numericInput("flat",
                                "Modifikator",
                                value = 0))
          ),
          
          numericInput(
            "speed",
            "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
            min = 1,
            value = 1
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
          
          actionButton("button", "➕ Weitere Waffe hinzufügen")
        ),
        bsCollapsePanel(
          "weapon_2",
          fluidRow(
            column(4,
                   numericInput(
                     "d6_2",
                     "Anzahl W6",
                     min = 0,
                     max = 8,
                     value = 0
                   )),
            column(4,
                   numericInput(
                     "d10_2",
                     "Anzahl W10",
                     min = 0,
                     max = 5,
                     value = 0
                   )),
            column(4,
                   numericInput("flat_2",
                                "Modifikator",
                                value = 0))
          ),
          
          numericInput(
            "speed_2",
            "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
            min = 1,
            value = 1
          ),
          
          ## Textoutput ----
          br(),
          textOutput("weapon_2"),
          # br(),
          textOutput("mean_dmg_2"),
          # br(),
          textOutput("mean_dmg_per_tick_2"),
          # br(),
          textOutput("sd_dmg_2"),
          br(),
          
          ## Waffenmerkmale ----
          h4("Waffenmerkmale:"),
          fluidRow(
            column(
              6,
              sliderInput(
                "exact_2",
                "Exakt",
                min = 0,
                max = 5,
                value = 0
              ),
              bsTooltip(
                id = "exact_2",
                title = "Bei einem Schadenswurf mit dieser Waffe werden so viele Würfel zusätzlich geworfen, wie die Stufe des Merkmals beträgt. Die höchsten Ergebnisse zählen für den Schadenswurf.",
                trigger = "hover"
              ),
              sliderInput(
                "critical_2",
                "Kritisch",
                min = 0,
                max = 5,
                value = 0
              ),
              bsTooltip(
                id = "critical_2",
                title = "Der Schaden eines Angriffs einer Waffe mit diesem Merkmal erhöht sich für jeden Schadenswürfel, der die maximale Augenzahl würfelt, um die Stufe des Merkmals.",
                trigger = "hover"
              )
            ),
            column(
              6,
              sliderInput(
                "penetration_2",
                "Durchdringung",
                min = 0,
                max = 6,
                value = 0
              ),
              bsTooltip(
                id = "penetration_2",
                title = "Für jede Stufe dieses Merkmals kann 1 Punkt der gegnerischen Schadensreduktion ignoriert werden, egal aus welcher Quelle diese stammt.",
                trigger = "hover"
              ),
              sliderInput(
                "sharp_2",
                "Scharf",
                min = 0,
                max = 5,
                value = 0
              ),
              bsTooltip(
                id = "sharp_2",
                title = "Alle Schadenswürfel einer Waffe mit diesem Merkmal werden immer als mindestens der Wert der Stufe des Merkmals gewertet, egal was eigentlich gewürfelt wurde.",
                trigger = "hover"
              )
            )
          ),
          br(),
          actionButton("button_2", "Zurück zu Waffe 1")
        )
      ),
      
      # Schadensreduktion ----
      conditionalPanel(
        condition = "input.tab_selected == 1",
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
          trigger = "hover"
        )
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 2",
        h4("Grenzen der Schadensreduktion:"),
        column(
          6,
          numericInput(
            "lower_bound",
            "Untere Grenze",
            min = 0,
            max = 9,
            value = 0
          )
        ),
        column(
          6,
          numericInput(
            "upper_bound",
            "Obere Grenze",
            min = 1,
            max = 25,
            value = 10
          )
        )
      ),
      
      br(),
      actionButton("reset_input", "Eingabe zurücksetzen"),
    ),
    
    
    ## Main-Panel ----
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel(
        "Wahrscheinlichkeiten",
        value = 1,
        plotOutput("dist_plot", width = "100%", height = "320px"),
        br(),
        selectInput(
          "y_axis",
          "Darstellung der kumulierten Grafik",
          choices = list(
            "mindestens x oder höher" = "cum_prob_min",
            "maximal x oder niedriger" = "cum_prob_max"
          )
        ),
        plotOutput("cum_dist_plot", width = "85%", height = "320px")
      ),
      tabPanel(
        "Schadensreduktion",
        value = 2,
        plotOutput("dmgred_plot", width = "85%", height = "450px"),
        selectInput(
          "y_axis_dr",
          "Art des durchschnittlichen Schadens",
          choices = list(
            "Durchschnittlicher Schaden" = "total",
            "Durchschnittlicher Schaden pro Tick" = "norm"
          )
        )
      ),
      id = "tab_selected"
    ))
  )
)


# Backend ----
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Tab 1 ----
  
  observeEvent(input$button, ({
    updateCollapse(session, "weapons", open = "weapon_2")
  }))
  observeEvent(input$button_2, ({
    updateCollapse(session, "weapons", open = "weapon_1")
  }))
  
  prob_vec <-
    reactive({
      req(any(c(input$d6, input$d10) != 0))
      convolve_vecs(
        n_d6 = input$d6,
        n_d10 = input$d10,
        att_exact = input$exact,
        att_sharp = input$sharp,
        att_critical = input$critical
      )
    })
  
  prob_vec_2 <-
    reactive({
      req(any(c(input$d6_2, input$d10_2) != 0))
      convolve_vecs(
        n_d6 = input$d6_2,
        n_d10 = input$d10_2,
        att_exact = input$exact_2,
        att_sharp = input$sharp_2,
        att_critical = input$critical_2
      )
    })
  
  prob_table <- reactive({
    if (isTruthy(any(c(input$d6, input$d10) != 0))) {
      create_prob_table(
        prob_vec(),
        flat_mod = input$flat,
        att_penetration = input$penetration,
        damage_reduction = input$dmg_reduction
      )[, weapon := "Waffe 1"]
    } else {
      req(any(c(input$d6, input$d10) != 0))
    }
  })
  
  prob_table_2 <- reactive({
    if (isTruthy(any(c(input$d6_2, input$d10_2) != 0))) {
      create_prob_table(
        prob_vec_2(),
        flat_mod = input$flat_2,
        att_penetration = input$penetration_2,
        damage_reduction = input$dmg_reduction
      )[, weapon := "Waffe 2"]
    } else {
      req(any(c(input$d6_2, input$d10_2) != 0))
    }
  })
  
  table <- reactive({
    req(isTruthy(prob_table()) && isTruthy(prob_table_2()))
    rbindlist(list("Waffe 1" = prob_table(), "Waffe 2" = prob_table_2()))
  })
  
  ## Textoutput ----
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
  mean_damage <-
    reactive(round(prob_table()[, sum(damage * probability)], 2))
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
        # FIXME
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
  
  ## Plots ----
  # Plot Probability Distribution
  output$dist_plot <- renderPlot({
    x <- tryCatch(
      table(),
      error = function(e)
        prob_table()
    )
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = probability, fill = weapon)) +
      geom_bar(
        stat = "identity",
        color = "black",
        position = position_dodge2(preserve = "single")
      ) +
      geom_text(aes(label = paste0(round(
        probability * 100, 1
      ), "%")),
      vjust = -0.3,
      position = position_dodge(width = 0.9)) + # FIXME In DT
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
    # x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = switch(
      input$y_axis,
      cum_prob_min = cum_prob_min,
      cum_prob_max = cum_prob_max
    ))) +
      geom_bar(stat = "identity",
               color = "black",
               fill = "dodgerblue1") +
      geom_text(aes(label = switch(
        input$y_axis,
        cum_prob_min = paste0(round(cum_prob_min * 100, 1), "%"),
        # FIXME In DT
        cum_prob_max = paste0(round(cum_prob_max * 100, 1), "%") # FIXME In DT
      )), vjust = -0.3) +
      ggtitle(switch(
        input$y_axis,
        cum_prob_min = "Kumulierte Wahrscheinlichkeiten (Mindestschaden)",
        cum_prob_max = "Kumulierte Wahrscheinlichkeiten (Maximalschaden)"
      )) +
      xlab("Schaden") +
      ylab("Wahrscheinlichkeit in %") +
      # scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_y_continuous(
        labels = function(x)
          paste0(x * 100, "%"),
        breaks = pretty_breaks(n = 10)
      ) +
      theme_classic(base_size = 20)
  })
  
  observe({
    if (all(c(input$d6, input$d10, input$d6_2, input$d10_2) == 0)) {
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
  
  # Tab 2 ----
  
  observe(updateSliderInput(session, "lower_bound", max = input$upper_bound - 1))
  
  dmgred_table <- reactive({
    req(any(c(input$d6, input$d10) != 0))
    create_dmgred_table(
      prob_vec = prob_vec(),
      flat_mod = input$flat,
      att_penetration = input$penetration,
      lower_bound = input$lower_bound,
      upper_bound = input$upper_bound
    )
  })
  
  ## Plots ----
  output$dmgred_plot <- renderPlot({
    x <- dmgred_table()
    # x_axis_labels <- min(x[, damage_reduction]):max(x[, damage_reduction])
    ggplot(data = x, aes(x = damage_reduction, y = switch(
      input$y_axis_dr,
      total = means,
      norm = means / input$speed
    ))) +
      geom_bar(stat = "identity",
               color = "black",
               fill = "orange") +
      geom_text(aes(label = switch(
        input$y_axis_dr,
        total = round(means, 2),
        # FIXME In DT
        norm = round(means / input$speed, 2)
      ), vjust = -0.3)) + # FIXME In DT
      ggtitle("Durchschn. Schaden nach Schadensreduktion des Gegners") +
      xlab("Schadensreduktion") +
      ylab(switch(input$y_axis_dr,
                  total = "Durchschn. Schaden",
                  norm = "Durchschn. Schaden / Tick")) +
      scale_x_continuous(breaks = x$damage_reduction) +
      #
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
                         # sec.axis = sec_axis( trans = ~. / input$speed, name = "D. Schaden / Tick")
                         #   labels = function(x)
                         #     paste0(x * 100, "%"),
                         #   breaks = pretty_breaks(n = 10)) +
                         theme_classic(base_size = 20)
                         })
    
    
    
    observeEvent(input$reset_input, {
      shinyjs::reset("side-panel")
    })
    
    }

# Run the application
shinyApp(ui = ui, server = server)
