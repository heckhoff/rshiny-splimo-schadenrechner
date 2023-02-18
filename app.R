library(shiny)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(scales)
library(stringi)

source("damage_calculation.R")
options(encoding = "UTF-8")

# Predefine y-axis plot choices (necessary due to special symbol in named list)
y_axis_choice <- setNames(
  list(
    "cum_prob_min",
    "cum_prob_max"
  ),
  c(
    paste0(
      "mindestens x oder h",
      stringi::stri_unescape_unicode("\U00F6"),
      "her"
    ),
    "maximal x oder niedriger"
  )
)

# Frontend ----

ui <- fluidPage(
  # theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  titlePanel("Splittermond Waffenschadenrechner"),

  ## Side-Panel ----
  sidebarLayout(
    sidebarPanel(
      shinyWidgets::setSliderColor(
        c(
          "#56B4E9",
          "#56B4E9",
          "#56B4E9",
          "#56B4E9",
          "#D55E00",
          "#D55E00",
          "#D55E00",
          "#D55E00",
          "black",
          "black"
        ),
        c(1:10)
      ),
      id = "side-panel",
      fluidRow(
        div(
          class = "col-xs-6",
          actionButton(
            "weapon_toggle",
            "Weitere Waffe",
            icon = icon("plus"),
            style = "font-size:115%;"
          ),
          style = "padding-bottom:15px;",
          align = "left"
        ),
        div(
          class = "col-xs-6",
          actionButton("reset_input", "",
            icon = icon("xmark"),
            style = "font-size:115%;"
          ),
          style = "padding-bottom:15px;",
          align = "right"
        )
      ),
      conditionalPanel(
        condition = "input.weapon_toggle % 2 == 0",
        fluidRow(column(
          12,
          pickerInput(
            "select_weapon_1",
            choices = data[, name],
            multiple = TRUE,
            options = pickerOptions(
              maxOptions = 1,
              liveSearch = TRUE,
              noneSelectedText = "Waffenpreset laden"
            ),
            width = "100%"
          )
        )),
        fluidRow(
          column(
            4,
            numericInput(
              "d6",
              "Anzahl W6",
              min = 0,
              max = 6,
              value = 0
            )
          ),
          column(
            4,
            numericInput(
              "d10",
              "Anzahl W10",
              min = 0,
              max = 5,
              value = 0
            )
          ),
          column(
            4,
            numericInput("flat",
              "Modifikator",
              value = 0
            )
          )
        ),
        fluidRow(column(
          12,
          numericInput(
            "speed",
            "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
            min = 1,
            value = 1
          )
        )),
        ## Textoutput ----
        htmlOutput("weapon"),
        htmlOutput("mean_dmg"),
        htmlOutput("mean_dmg_per_tick"),
        htmlOutput("sd_dmg"),
        br(),
        ## Waffenmerkmale ----
        fluidRow(column(
          12,
          actionButton(
            "properties_toggle",
            "Waffenmerkmale",
            icon = icon("minus"),
            width = "200px",
            style = "font-size:125%;"
          )
        )),
        conditionalPanel(
          condition = "input.properties_toggle % 2 == 0",
          fluidRow(
            column(
              6,
              sliderInput(
                "exact",
                "Exakt",
                min = 0,
                max = 3,
                value = 0
              ),
              bsTooltip(
                id = "exact",
                title = "Bei einem Schadenswurf mit dieser Waffe werden so viele Würfel zusätzlich geworfen, wie die Stufe des Merkmals beträgt. Die höchsten Ergebnisse zählen für den Schadenswurf.",
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
              )
            ),
            style = "padding-top:10px"
          ),
          fluidRow(
            column(
              6,
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
          fluidRow(
            column(
              6,
              materialSwitch("massive",
                "Wuchtig",
                value = FALSE,
                status = "primary"
              ),
              bsTooltip(
                id = "massive",
                title = "Bei Waffen mit diesem Merkmal verursacht das freie Manöver Wuchtangriff 2 statt 1 zusätzlichen Punkt Schaden pro eingesetzten Erfolgsgrad.",
                trigger = "hover"
              )
            ),
            column(
              6,
              materialSwitch(
                "versatile",
                "Vielseitig",
                value = FALSE,
                status = "primary"
              ),
              bsTooltip(
                # FIXME
                id = "wield",
                title = "Bei vielseitigen Waffen kann zwischen einhändiger und zweihändiger Führung gewechselt werden. In zweihändiger Führung erhöht sich ihr
Schaden um 3 Punkte.",
                trigger = "hover"
              ),
              radioGroupButtons(
                inputId = "wield",
                choices = c("Einh&#228ndig" = FALSE, "Zweih&#228ndig" = TRUE),
                status = "primary"
              )
            )
          )
        ),
        br()
      ),
      conditionalPanel(
        condition = "input.weapon_toggle % 2 == 1",
        fluidRow(
          column(
            12,
            pickerInput(
              "select_weapon_2",
              choices = data[, name],
              multiple = TRUE,
              options = pickerOptions(
                maxOptions = 1,
                liveSearch = TRUE,
                noneSelectedText = "Waffenpreset laden"
              ),
              width = "100%"
            )
          ),
          align = "right"
        ),
        fluidRow(
          column(
            4,
            numericInput(
              "d6_2",
              "Anzahl W6",
              min = 0,
              max = 8,
              value = 0
            )
          ),
          column(
            4,
            numericInput(
              "d10_2",
              "Anzahl W10",
              min = 0,
              max = 5,
              value = 0
            )
          ),
          column(
            4,
            numericInput("flat_2",
              "Modifikator",
              value = 0
            )
          )
        ),
        fluidRow(column(
          12,
          numericInput(
            "speed_2",
            "Waffengeschwindigkeit inkl. weiterer Modifikatoren (z.B. +3 Ticks bei Fernkampf)",
            min = 1,
            value = 1
          )
        )),

        ## Textoutput ----
        htmlOutput("weapon_2"),
        htmlOutput("mean_dmg_2"),
        htmlOutput("mean_dmg_per_tick_2"),
        htmlOutput("sd_dmg_2"),
        br(),

        ## Waffenmerkmale ----
        fluidRow(column(
          12,
          actionButton(
            "properties_toggle_2",
            "Waffenmerkmale",
            icon = icon("minus"),
            width = "200px",
            style = "font-size:125%;"
          )
        )),
        conditionalPanel(
          condition = "input.properties_toggle_2 % 2 == 0",
          fluidRow(
            column(
              6,
              sliderInput(
                "exact_2",
                "Exakt",
                min = 0,
                max = 3,
                value = 0
              ),
              bsTooltip(
                id = "exact_2",
                title = "Bei einem Schadenswurf mit dieser Waffe werden so viele Würfel zusätzlich geworfen, wie die Stufe des Merkmals beträgt. Die höchsten Ergebnisse zählen für den Schadenswurf.",
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
              )
            ),
            style = "padding-top:10px"
          ),
          fluidRow(
            column(
              6,
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
          fluidRow(
            div(
              class = "col-xs-6",
              materialSwitch("massive_2",
                "Wuchtig",
                value = FALSE,
                status = "primary"
              ),
              bsTooltip(
                id = "massive_2",
                title = "Bei Waffen mit diesem Merkmal verursacht das freie Manöver Wuchtangriff 2 statt 1 zusätzlichen Punkt Schaden pro eingesetzten Erfolgsgrad.",
                trigger = "hover"
              )
            ),
            div(
              class = "col-xs-6",
              materialSwitch(
                "versatile_2",
                "Vielseitig",
                value = FALSE,
                status = "primary"
              ),
              bsTooltip(
                # FIXME
                id = "wield_2",
                title = "Bei vielseitigen Waffen kann zwischen einhändiger und zweihändiger Führung gewechselt werden. In zweihändiger Führung erhöht sich ihr
Schaden um 3 Punkte.",
                trigger = "hover"
              ),
              radioGroupButtons(
                inputId = "wield_2",
                choices = c("Einh&#228ndig" = FALSE, "Zweih&#228ndig" = TRUE),
                status = "primary"
              )
            )
          )
        ),
        br()
      ),
      fluidRow(column(
        12,
        actionButton(
          "modifiers_toggle",
          "Weitere Parameter",
          icon = icon("plus"),
          width = "200px",
          style = "font-size:125%;"
        )
      )),
      # Schadensreduktion ----
      conditionalPanel(
        condition = "input.tab_selected == 1 & input.modifiers_toggle % 2 == 1",
        br(),
        fluidRow(column(
          12,
          sliderInput(
            "dmg_reduction",
            "Simulierte Schadensreduktion",
            min = 0,
            max = 10,
            value = 0
          )
        )),
        bsTooltip(
          id = "dmg_reduction",
          title = "Die Schadensreduktion einer Rüstung wird von dem Schaden jedes erfolgreichen Angriffs gegen den Träger abgezogen.",
          trigger = "hover"
        ),
        fluidRow(column(
          12,
          sliderInput(
            "success_lvl",
            "Für Wuchtangriff genutzte Erfolgsgrade",
            min = 0,
            max = 10,
            value = 0
          )
        )),
        bsTooltip(
          id = "success_lvl",
          title = "Für jeden aufgewendeten Erfolgsgrad richtet der Angriff einen zusätzlichen Punkt Schaden an.",
          trigger = "hover"
        )
      ),
      conditionalPanel(
        condition = "input.tab_selected == 2  & input.modifiers_toggle % 2 == 1",
        br(),
        h4("Grenzen der Schadensreduktion/Erfolgsgrade:"),
        fluidRow(
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
        )
      ),
      br(),
      width = 4
    ),


    ## Main-Panel ----
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Wahrscheinlichkeiten",
          value = 1,
          plotOutput("dist_plot", height = "340px"),
          # br(),
          selectInput("y_axis",
            "Darstellung der kumulierten Grafik",
            choices = y_axis_choice
          ),
          plotOutput("cum_dist_plot", height = "340px")
        ),
        tabPanel(
          "Schadensreduktion",
          value = 2,
          plotOutput("dmgred_plot", height = "340px"),
          # br(),
          plotOutput("slvls_plot", height = "340px"),
          selectInput(
            "y_axis_dr",
            "Art des durchschnittlichen Schadens",
            choices = list(
              "Durchschnittlicher Schaden" = "total",
              "Durchschnittlicher Schaden pro Tick" = "norm"
            ),
            selected = "norm"
          )
        ),
        id = "tab_selected"
      ),
      width = 7
    )
  )
)


server <- function(input, output, session) {
  # Hide objects
  hide("weapon_toggle")
  hide("wield")
  hide("wield_2")

  # Tab 1 ----

  ## Update elements ----
  observeEvent(reactiveValuesToList(input),
    {
      show("weapon_toggle")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$properties_toggle, {
    updateActionButton(session,
      "properties_toggle",
      icon = if ((input$properties_toggle %% 2) == 0) {
        icon("minus")
      } else {
        icon("plus")
      }
    )
  })

  observeEvent(input$properties_toggle_2, {
    updateActionButton(session,
      "properties_toggle_2",
      icon = if ((input$properties_toggle_2 %% 2) == 0) {
        icon("minus")
      } else {
        icon("plus")
      }
    )
  })

  observeEvent(input$modifiers_toggle, {
    updateActionButton(session,
      "modifiers_toggle",
      icon = if ((input$modifiers_toggle %% 2) == 0) {
        icon("plus")
      } else {
        icon("minus")
      }
    )
  })

  observeEvent(input$weapon_toggle, {
    updateActionButton(
      session,
      "weapon_toggle",
      label = ifelse((input$weapon_toggle %% 2) == 0,
        "Waffe 2 anpassen",
        "Waffe 1 anpassen"
      ),
      icon = icon(NULL)
    )
  })

  ## Create objects ----

  weapon_1_name <- reactiveVal(value = "Waffe 1")
  weapon_2_name <- reactiveVal(value = "Waffe 2")

  weapon_colors <-
    reactive({
      setNames(
        c("#56B4E9", "#D55E00"),
        c(weapon_1_name(), weapon_2_name())
      )
    })

  sel_weapon_1 <- reactive({
    data[name %in% input$select_weapon_1, .SD, .SDcols = c(
      "n_d6",
      "n_d10",
      "flat_mod",
      "speed",
      "durchdringung",
      "exakt",
      "kritisch",
      "scharf",
      "vielseitig",
      "wuchtig"
    )]
  })

  sel_weapon_2 <- reactive({
    data[name %in% input$select_weapon_2, .SD, .SDcols = c(
      "n_d6",
      "n_d10",
      "flat_mod",
      "speed",
      "durchdringung",
      "exakt",
      "kritisch",
      "scharf",
      "vielseitig",
      "wuchtig"
    )]
  })

  input_weapon_1 <- reactive({
    data.table(
      n_d6 = input$d6,
      n_d10 = input$d10,
      flat_mod = input$flat,
      speed = input$speed,
      durchdringung = input$penetration,
      exakt = input$exact,
      kritisch = input$critical,
      scharf = input$sharp,
      vielseitig = input$versatile,
      wuchtig = input$massive
    )
  })

  input_weapon_2 <- reactive({
    data.table(
      n_d6 = input$d6_2,
      n_d10 = input$d10_2,
      flat_mod = input$flat_2,
      speed = input$speed_2,
      durchdringung = input$penetration_2,
      exakt = input$exact_2,
      kritisch = input$critical_2,
      scharf = input$sharp_2,
      vielseitig = input$versatile_2,
      wuchtig = input$massive_2
    )
  })

  preset_input_diff_1 <- reactive({
    isTRUE(all.equal(sel_weapon_1(), input_weapon_1()))
  })

  preset_input_diff_2 <- reactive({
    isTRUE(all.equal(sel_weapon_2(), input_weapon_2()))
  })

  comb_1 <-
    reactive({
      req(any(c(input$d6, input$d10) != 0))
      combine_dice(
        n_d6 = input$d6,
        n_d10 = input$d10,
        att_exact = input$exact,
        att_sharp = input$sharp,
        att_critical = input$critical
      )
    })

  comb_2 <-
    reactive({
      req(any(c(input$d6_2, input$d10_2) != 0))
      combine_dice(
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
        comb_1(),
        flat_mod = input$flat,
        att_penetration = input$penetration,
        damage_reduction = input$dmg_reduction,
        success_level = input$success_lvl,
        att_massive = input$massive,
        att_versatile = input$wield
      )[, name := factor(x = weapon_1_name())]
    } else {
      # TODO Is this correct?
      req(any(c(input$d6, input$d10) != 0))
    }
  })

  prob_table_2 <- reactive({
    if (isTruthy(any(c(input$d6_2, input$d10_2) != 0))) {
      create_prob_table(
        comb_2(),
        flat_mod = input$flat_2,
        att_penetration = input$penetration_2,
        damage_reduction = input$dmg_reduction,
        success_level = input$success_lvl,
        att_massive = input$massive_2,
        att_versatile = input$wield_2
      )[, name := factor(weapon_2_name())]
    } else {
      req(any(c(input$d6_2, input$d10_2) != 0))
    }
  })

  prob_tables <- reactive({
    req(isTruthy(prob_table()) && isTruthy(prob_table_2()))
    rbindlist(list("Waffe 1" = prob_table(), "Waffe 2" = prob_table_2()))
  })

  ## Textoutput ----
  # FIXME Print like sd_damage (in one)
  # Print Selected Weapon
  print_weapon_txt <-
    reactive(
      paste0(
        "<b><FONT COLOR='#56B4E9'>Ausgew&auml;hlte Waffe: </FONT COLOR></b>",
        create_weapon_txt(input$d6, input$d10, input$flat)
      )
    )
  output$weapon <- renderText({
    print_weapon_txt()
  })

  print_weapon_txt_2 <-
    reactive(
      paste0(
        "<b><FONT COLOR='D55E00'>Ausgew&auml;hlte Waffe: </FONT COLOR></b>",
        create_weapon_txt(input$d6_2, input$d10_2, input$flat_2)
      )
    )
  output$weapon_2 <- renderText({
    print_weapon_txt_2()
  })

  # Calculate Average Damage
  mean_damage <-
    reactive(round(prob_table()[, sum(damage * probability)], 2))
  # Print Average Damage
  print_mean_damage <-
    reactive(
      paste(
        "<b><FONT COLOR='#56B4E9'>Durchschnittlicher Schaden:</FONT COLOR></b>",
        mean_damage()
      )
    )
  output$mean_dmg <- renderText({
    print_mean_damage()
  })

  mean_damage_2 <-
    reactive(round(prob_table_2()[, sum(damage * probability)], 2))
  # Print Average Damage
  print_mean_damage_2 <-
    reactive(
      paste(
        "<b><FONT COLOR='D55E00'>Durchschnittlicher Schaden:</FONT COLOR></b>",
        mean_damage_2()
      )
    )
  output$mean_dmg_2 <- renderText({
    print_mean_damage_2()
  })

  # Print Average Damage per Tick
  print_mean_damage_per_tick <-
    reactive(
      paste(
        "<b><FONT COLOR='#56B4E9'>Durchschn. Schaden pro Tick:</FONT COLOR></b>",
        fifelse(
          input$speed != 0,
          # FIXME
          yes =
            round(prob_table()[, sum(damage * probability)] / input$speed, 2),
          no = 0
        )
      )
    )
  output$mean_dmg_per_tick <- renderText({
    print_mean_damage_per_tick()
  })

  print_mean_damage_per_tick_2 <-
    reactive(
      paste(
        "<b><FONT COLOR='D55E00'>Durchschn. Schaden pro Tick:</FONT COLOR></b>",
        fifelse(
          input$speed_2 != 0,
          # FIXME
          yes =
            round(prob_table_2()[, sum(damage * probability)] / input$speed_2, 2),
          no = 0
        )
      )
    )
  output$mean_dmg_per_tick_2 <- renderText({
    print_mean_damage_per_tick_2()
  })

  # Print Damage Standard Deviation
  sd_damage <-
    reactive(
      paste(
        "<b><FONT COLOR='#56B4E9'>Durchschnittliche Abweichung:</FONT COLOR></b>",
        round(prob_table()
        [, sqrt(sum((damage - sum(
            damage * probability
          ))
          ^2 * probability))], 2)
      )
    )
  output$sd_dmg <- renderText({
    sd_damage()
  })


  sd_damage_2 <-
    reactive(
      paste(
        "<b><FONT COLOR='D55E00'>Durchschnittliche Abweichung:</FONT COLOR></b>",
        round(prob_table_2()
        [, sqrt(sum((damage - sum(
            damage * probability
          ))
          ^2 * probability))], 2)
      )
    )
  output$sd_dmg_2 <- renderText({
    sd_damage_2()
  })

  ## Plots ----
  # Plot Probability Distribution
  # TODO Optimize
  output$dist_plot <- renderPlot({
    x <- tryCatch(
      prob_tables(),
      error = function(e) {
        prob_table()
      }
    )
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = probability, fill = name)) +
      geom_bar(
        stat = "identity",
        color = "black",
        position = position_dodge2(preserve = "single")
      ) +
      scale_fill_manual(values = weapon_colors()) +
      geom_text(
        aes(label = round(probability * 100, 1)),
        vjust = -0.3,
        position = position_dodge(width = 0.9),
        fontface = 2
      ) + # FIXME In DT
      ggtitle("Wahrscheinlichkeitsverteilung des Schadens") +
      xlab("Schaden") +
      ylab("Wahrscheinlichkeit in %") +
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_y_continuous(
        labels = function(x) {
          paste0(x * 100, "%")
        },
        breaks = pretty_breaks(n = 10),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(fill = "Waffe") +
      theme_classic(base_size = 20) +
      theme(legend.position = ifelse(
        any(weapon_2_name() %in% x$name),
        yes = "bottom",
        no = "none"
      ))
  })

  # Plot Cumulative Probability Distribution
  output$cum_dist_plot <- renderPlot({
    x <- tryCatch(
      prob_tables(),
      error = function(e) {
        prob_table()
      }
    )
    x_axis_labels <- min(x[, damage]):max(x[, damage])
    ggplot(data = x, aes(x = damage, y = switch(input$y_axis,
      cum_prob_min = cum_prob_min,
      cum_prob_max = cum_prob_max
    ), fill = name)) +
      geom_bar(
        stat = "identity",
        color = "black",
        position = position_dodge2(preserve = "single")
      ) +
      scale_fill_manual(values = weapon_colors()) +
      geom_text(
        aes(label = switch(input$y_axis,
          cum_prob_min = round(cum_prob_min * 100, 1),
          # FIXME In DT
          cum_prob_max = round(cum_prob_max * 100, 1) # FIXME In DT
        ), fontface = 2),
        vjust = -0.3,
        position = position_dodge(width = 0.9)
      ) +
      ggtitle(switch(input$y_axis,
        cum_prob_min = "Kumulierte Wahrscheinlichkeiten (Mindestschaden)",
        cum_prob_max = "Kumulierte Wahrscheinlichkeiten (Maximalschaden)"
      )) +
      xlab("Schaden") +
      ylab("Wahrscheinlichkeit in %") +
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_y_continuous(
        labels = function(x) {
          paste0(x * 100, "%")
        },
        breaks = pretty_breaks(n = 10),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(fill = "Waffe") +
      theme_classic(base_size = 20) +
      # TODO Optimize in all plots
      theme(legend.position = ifelse(
        any(weapon_2_name() %in% x$name),
        yes = "bottom",
        no = "none"
      ))
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

  observe({
    if (all(c(input$d6_2, input$d10_2) == 0)) {
      hide("weapon_2")
      hide("mean_dmg_2")
    } else {
      show("weapon_2")
      show("mean_dmg_2")
    }
  })

  # Tab 2 ----

  observe(updateSliderInput(session, "lower_bound", max = input$upper_bound - 1))

  dmgred_table <- reactive({
    if (isTruthy(any(c(input$d6, input$d10) != 0))) {
      create_dmgred_table(
        comb_1(),
        flat_mod = input$flat,
        weapon_speed = input$speed,
        att_penetration = input$penetration,
        success_level = input$success_lvl,
        att_massive = input$massive,
        att_versatile = input$wield,
        lower_bound = input$lower_bound,
        upper_bound = input$upper_bound
      )[, name := weapon_1_name()]
    } else {
      req(any(c(input$d6, input$d10) != 0))
    }
  })

  dmgred_table_2 <- reactive({
    if (isTruthy(any(c(input$d6_2, input$d10_2) != 0))) {
      create_dmgred_table(
        comb_2(),
        flat_mod = input$flat_2,
        weapon_speed = input$speed_2,
        att_penetration = input$penetration_2,
        success_level = input$success_lvl,
        att_massive = input$massive_2,
        att_versatile = input$wield_2,
        lower_bound = input$lower_bound,
        upper_bound = input$upper_bound
      )[, name := weapon_2_name()]
    } else {
      req(any(c(input$d6_2, input$d10_2) != 0))
    }
  })

  dmgred_tables <- reactive({
    req(isTruthy(dmgred_table()) && isTruthy(dmgred_table_2()))
    rbindlist(list("Waffe 1" = dmgred_table(), "Waffe 2" = dmgred_table_2()))
  })


  slvls_table <- reactive({
    if (isTruthy(any(c(input$d6, input$d10) != 0))) {
      create_slvls_table(
        comb_1(),
        flat_mod = input$flat,
        weapon_speed = input$speed,
        damage_reduction = input$dmg_reduction,
        att_penetration = input$penetration,
        att_massive = input$massive,
        att_versatile = input$wield,
        lower_bound = input$lower_bound,
        upper_bound = input$upper_bound
      )[, name := weapon_1_name()]
    } else {
      req(any(c(input$d6, input$d10) != 0))
    }
  })

  slvls_table_2 <- reactive({
    if (isTruthy(any(c(input$d6_2, input$d10_2) != 0))) {
      create_slvls_table(
        comb_2(),
        flat_mod = input$flat_2,
        weapon_speed = input$speed_2,
        damage_reduction = input$dmg_reduction,
        att_penetration = input$penetration_2,
        att_massive = input$massive_2,
        att_versatile = input$wield_2,
        lower_bound = input$lower_bound,
        upper_bound = input$upper_bound
      )[, name := weapon_2_name()]
    } else {
      req(any(c(input$d6_2, input$d10_2) != 0))
    }
  })

  slvls_tables <- reactive({
    req(isTruthy(slvls_table()) && isTruthy(slvls_table_2()))
    rbindlist(list("Waffe 1" = slvls_table(), "Waffe 2" = slvls_table_2()))
  })

  ## Plots ----

  output$dmgred_plot <- renderPlot({
    x <- tryCatch(
      dmgred_tables(),
      error = function(e) {
        dmgred_table()
      }
    )
    x_axis_labels <-
      min(x[, damage_reduction]):max(x[, damage_reduction])
    ggplot(data = x, aes(x = damage_reduction, y = switch(input$y_axis_dr,
      total = means,
      norm = means_per_tick
    ), fill = name)) +
      geom_bar(
        stat = "identity",
        color = "black",
        position = position_dodge2(preserve = "single")
      ) +
      scale_fill_manual(values = weapon_colors()) +
      geom_text(
        aes(label = switch(input$y_axis_dr,
          total = round(means, 1),
          # FIXME In DT
          norm = round(means_per_tick, 2)
        ), fontface = 2),
        vjust = -0.3,
        position = position_dodge(width = 0.9)
      ) + # FIXME In DT
      ggtitle("Durchschn. Schaden nach Schadensreduktion des Gegners") +
      xlab("Schadensreduktion") +
      ylab(switch(input$y_axis_dr,
        total = "Durchschn. Schaden",
        norm = "D. Schaden / Tick"
      )) +
      labs(fill = "Waffe") +
      scale_x_continuous(breaks = x$damage_reduction) +
      scale_y_continuous(
        breaks = pretty_breaks(n = 10),
        expand = expansion(mult = c(0, 0.08))
      ) +
      theme_classic(base_size = 20) +
      theme(legend.position = ifelse(
        any(weapon_2_name() %in% x$name),
        yes = "bottom",
        no = "none"
      ))
  })

  output$slvls_plot <- renderPlot({
    x <- tryCatch(
      slvls_tables(),
      error = function(e) {
        slvls_table()
      }
    )
    x_axis_labels <-
      min(x[, success_lvls]):max(x[, success_lvls])
    ggplot(data = x, aes(x = success_lvls, y = switch(input$y_axis_dr,
      total = means,
      norm = means_per_tick
    ), fill = name)) +
      geom_bar(
        stat = "identity",
        color = "black",
        position = position_dodge2(preserve = "single")
      ) +
      scale_fill_manual(values = weapon_colors()) +
      geom_text(
        aes(label = switch(input$y_axis_dr,
          total = round(means, 1),
          # FIXME In DT
          norm = round(means_per_tick, 2)
        ), fontface = 2),
        vjust = -0.3,
        position = position_dodge(width = 0.9)
      ) + # FIXME In DT
      ggtitle("Durchschn. Schaden nach für 'Wuchtangriff' eingesetzten EG") +
      xlab("Für Wuchtangriff eingesetzte EG") +
      ylab(switch(input$y_axis_dr,
        total = "Durchschn. Schaden",
        norm = "D. Schaden / Tick"
      )) +
      labs(fill = "Waffe") +
      scale_x_continuous(breaks = x$success_lvls) +
      scale_y_continuous(
        breaks = pretty_breaks(n = 10),
        expand = expansion(mult = c(0, 0.08))
      ) +
      theme_classic(base_size = 20) +
      theme(legend.position = ifelse(
        any(weapon_2_name() %in% x$name),
        yes = "bottom",
        no = "none"
      ))
  })

  # Update / Reset inputs ----

  ## Weapon 1 ----

  observeEvent(input$select_weapon_1, {
    weapon_1_name(input$select_weapon_1)
  })

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "d6", value = sel_weapon_1()$n_d6)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "d10", value = sel_weapon_1()$n_d10)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "flat", value = sel_weapon_1()$flat_mod)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "speed", value = sel_weapon_1()$speed)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "exact", value = sel_weapon_1()$exakt)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "critical", value = sel_weapon_1()$kritisch)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "penetration", value = sel_weapon_1()$durchdringung)
  )

  observeEvent(
    input$select_weapon_1,
    updateNumericInput(session, "sharp", value = sel_weapon_1()$scharf)
  )

  observeEvent(
    input$select_weapon_1,
    updateCheckboxInput(session, "massive", value = sel_weapon_1()$wuchtig)
  )

  observe(if (input$versatile == FALSE) {
    hide("wield")
    updateRadioGroupButtons(session, "wield", selected = FALSE)
  } else {
    show("wield")
    updateRadioGroupButtons(session, "wield", selected = TRUE)
  })

  observeEvent(
    input$select_weapon_1,
    updateMaterialSwitch(session, "versatile", value = sel_weapon_1()$vielseitig)
  )

  observeEvent(preset_input_diff_1(), {
    if (preset_input_diff_1() == FALSE) {
      weapon_1_name("Waffe 1")
    } else {
      weapon_1_name(input$select_weapon_1)
    }
  })

  ## Weapon 2 ----

  observeEvent(input$select_weapon_2, {
    weapon_2_name(input$select_weapon_2)
  })

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "d6_2", value = sel_weapon_2()$n_d6)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "d10_2", value = sel_weapon_2()$n_d10)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "flat_2", value = sel_weapon_2()$flat_mod)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "speed_2", value = sel_weapon_2()$speed)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "exact_2", value = sel_weapon_2()$exakt)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "critical_2", value = sel_weapon_2()$kritisch)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "penetration_2", value = sel_weapon_2()$durchdringung)
  )

  observeEvent(
    input$select_weapon_2,
    updateNumericInput(session, "sharp_2", value = sel_weapon_2()$scharf)
  )

  observeEvent(
    input$select_weapon_2,
    updateCheckboxInput(session, "massive_2", value = sel_weapon_2()$wuchtig)
  )

  observe(if (input$versatile_2 == FALSE) {
    hide("wield_2")
    updateRadioGroupButtons(session, "wield_2", selected = FALSE)
  } else {
    show("wield_2")
    updateRadioGroupButtons(session, "wield_2", selected = TRUE)
  })

  observeEvent(
    input$select_weapon_2,
    updateMaterialSwitch(session, "versatile_2", value = sel_weapon_2()$vielseitig)
  )

  observeEvent(preset_input_diff_2(), {
    if (preset_input_diff_2() == FALSE) {
      weapon_2_name("Waffe 2")
    } else {
      weapon_2_name(input$select_weapon_2)
    }
  })

  # TODO Refactor all to bindEvent()
  observeEvent(input$reset_input, {
    reset()
    # reset("weapon_toggle") # FIXME Not working
    weapon_1_name("Waffe 1")
    weapon_2_name("Waffe 2")
  })
}


# Run the application
shinyApp(ui = ui, server = server)
