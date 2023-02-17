library(data.table)
library(openxlsx)
library(doParallel)

options(scipen = 999)

# Online Resources ----
# https://anydice.com/
# https://shiny.rstudio.com/gallery/widget-gallery.html
# https://shiny.rstudio.com/gallery/button-styler.html
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
# https://www.dungeonsolvers.com/2018/03/29/average-dice-roll/
# https://stats.stackexchange.com/questions/177163/what-is-the-distribution-for-various-polyhedral-dice-all-rolled-at-once
# https://stackoverflow.com/questions/44339070/calculating-population-standard-deviation-in-r
# https://math.stackexchange.com/questions/1696623/what-is-the-expected-value-of-the-largest-of-the-three-dice-rolls
# https://shiny.rstudio.com/gallery/

# To-Dos ----
# Offen:
# FIXME Standardabweichung bei SR
# TODO Waffenmerkmale: vielseitig
# Vielseitige Nutzung entweder als Checkbox mitberücksichtigen und Werte
# updaten, oder bei Checkbox beide Outputs (ein- und zweihändige Führung)
# TODO Waffenbeschreibung um Liste "Merkmale: Scharf 1, Kritisch 2, ..." ergänzen
# TODO Standardabweichungen
# TODO Vergleich von Waffen in Plots ermöglichen
# TODO Tabellenoutput, RMarkdown Report mit allen Plots und Werten :)
# TODO Code effizienter gestalten, indem redundante Berechnungen (z.B. Round)
# direkt als cols im Table liegen
# TODO Fernkampfwaffen ebenfalls scrapen
# TODO Think about matrix functions, writing out sample space
#   https://www.statisticshowto.com/probability-and-statistics/probability-main-index/dice-roll-probability-6-sided-dice/
# TODO Code sinnvoll auf einzelne Files aufteilen, z.B. Plotberechnungen andernorts
# TODO Ersetzen seq.int mit seq_along(x:y)
# TODO Erfolgsgrad-Bestimmungen - Wuchtangriff (Merkmal wuchtig)
# Einfache Variante: Eingabe erwarteter Erfolgsgrade
# Schwierige Variante: Fähigkeitswert + Wurfoption Wahrscheinlichkeiten bestimmen
# und mit Wahrscheinlichkeiten der Schadensrechnung kombinieren
# (auch Fehlschläge in Durchschnittsschaden einfließen lassen etc.)
# TODO Gegnersimulation
# TODO bindEvent()
# TODO Reactive Ranges für die Bar Plots (Range Slider)
# TODO Waffenauswahl (Presets?)
# Abseits GitHub: Auswertungstabelle Mondstahlklingen mit allen Berechnungen
# TODO Trefferwahrscheinlichkeit
# TODO Vergleichsfunktion mehrere Plots / ineinander
# TODO Code kommentieren u. dokumentieren
# TODO README schreiben
# TODO
# TODO Bonuswürfel (z.B. Pfeilzauber wie Scharfer Wind mit anderen Attributen)

# Fertig:
# TODO alle Waffenwürfel
# TODO Flat Bonus
# TODO Kumulative Wahrscheinlichkeiten (min. X Schaden, Funktionsparameter?)
# TODO Reverse kumulativ (max. X Schaden)

# Import data ----

data <- read.xlsx("data/waffenliste_mondstahlklingen.xlsx", sheet = 1)
setDT(data)

# Dice Calculations ----

combine_dice <- function(n_d6 = 0,
                         n_d10 = 0,
                         att_exact = 0,
                         att_sharp = 0,
                         att_critical = 0,
                         cluster) {
  # browser()
  n_dice <- n_d6 + n_d10
  seq_d6 <-
    if (n_d6 > 0) {
      seq <- c(seq.int(1, 5), 6 + att_critical)
      seq[seq %in% seq_len(att_sharp)] <- att_sharp
      rep(list(seq), n_d6 + att_exact)
    } else {
      0
    }

  seq_d10 <-
    if (n_d10 > 0) {
      seq <- c(seq.int(1, 9), 10 + att_critical)
      seq[seq %in% seq_len(att_sharp)] <- att_sharp
      rep(list(seq), n_d10 + att_exact)
    } else {
      0
    }

  seqs <- c(seq_d6, seq_d10)

  clusterExport(cluster, varlist = c("seqs", "n_dice"), envir = environment())
  combinations <-
    sort(parApply(cl = cluster, expand.grid(seqs), 1, function(x) {
      sum(x[order(x, decreasing = TRUE, method = "shell")[seq_len(n_dice)]])
    })) # FIXME sort only if necessary

  # TODO Does simplyfing it like this change anything?
  # apply(expand.grid(seqs), 1, sum)
  # TODO Answer: Yes - doesn't take only highest dice when exact

  return(combinations)
}

# Text Constructions ----

create_weapon_txt <- function(n_d6 = 0,
                              n_d10 = 0,
                              flat_mod = 0) {
  fcase(
    n_d6 != 0 & n_d10 == 0,
    paste0(n_d6, "W6", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    }),
    n_d6 == 0 & n_d10 != 0,
    paste0(n_d10, "W10", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    }),
    default = paste0(n_d6, "W6+", n_d10, "W10", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    })
  )
}

# Plot Tables ----

create_prob_table <- function(combinations,
                              flat_mod = 0,
                              damage_reduction = 0,
                              att_penetration = 0,
                              success_level = 0,
                              att_massive = FALSE,
                              att_versatile = FALSE) {
  dmg_red <- pmax(0, damage_reduction - att_penetration)

  success <- ifelse(att_massive, success_level * 2, success_level)
  flat_mod <- ifelse(att_versatile, flat_mod + 3, flat_mod)

  dt <-
    data.table(
      damage = combinations,
      probability = 1 / length(combinations)
    )
  dt[, damage := pmax(0, damage + flat_mod + success - dmg_red)]
  dt <- dt[, lapply(.SD, sum), by = damage]
  dt[, cum_prob_min := rev(cumsum(rev(probability)))]
  dt[, cum_prob_max := cumsum(probability)]
  return(dt)
}

create_dmgred_table <- function(combinations,
                                flat_mod = 0,
                                weapon_speed = 1,
                                att_penetration = 0,
                                success_level = 0,
                                att_massive = FALSE,
                                att_versatile = FALSE,
                                lower_bound = 0,
                                upper_bound = 10) {
  damage_reduction <- seq.int(lower_bound, upper_bound)
  dmgred_pen_diff <- pmax(0, damage_reduction - att_penetration)

  success <- ifelse(att_massive, success_level * 2, success_level)
  flat_mod <- ifelse(att_versatile, flat_mod + 3, flat_mod)

  probs <-
    data.table(
      damage = pmax(0, combinations + flat_mod + success),
      probability = 1 / length(combinations)
    )
  probs <- probs[, lapply(.SD, sum), by = damage]

  means <-
    sapply(dmgred_pen_diff, function(x) {
      pmax(0, probs[, damage] - x)
    })
  means <- as.vector(probs[, probability] %*% means)

  dmgred_table <-
    data.table(
      damage_reduction = damage_reduction,
      means = means,
      means_per_tick = means / weapon_speed
    )

  return(dmgred_table)
}

create_slvls_table <- function(combinations,
                               flat_mod = 0,
                               weapon_speed = 1,
                               damage_reduction = 0,
                               att_penetration = 0,
                               att_massive = FALSE,
                               att_versatile = FALSE,
                               lower_bound = 0,
                               upper_bound = 10) {
  success_lvls <- seq.int(lower_bound, upper_bound)
  dmgred_pen_diff <- pmax(0, damage_reduction - att_penetration)

  flat_mod <- ifelse(att_versatile, flat_mod + 3, flat_mod)

  probs <-
    data.table(
      damage = pmax(0, combinations + flat_mod - dmgred_pen_diff),
      probability = 1 / length(combinations)
    )
  probs <- probs[, lapply(.SD, sum), by = damage]

  means <-
    sapply(success_lvls, function(x) {
      pmax(0, probs[, damage] + ifelse(att_massive, x * 2, x))
    })
  means <- as.vector(probs[, probability] %*% means)

  slvls_table <-
    data.table(
      success_lvls = success_lvls,
      means = means,
      means_per_tick = means / weapon_speed
    )

  return(slvls_table)
}
