library(ggplot2)
library(data.table)

options(scipen = 999)

# https://anydice.com/
# https://www.dungeonsolvers.com/2018/03/29/average-dice-roll/
# https://stats.stackexchange.com/questions/177163/what-is-the-distribution-for-various-polyhedral-dice-all-rolled-at-once
# https://stackoverflow.com/questions/44339070/calculating-population-standard-deviation-in-r

# To-Dos ----
# Offen:
# PRIORITY ----
# Bugfix: sharp != Sharp
# Bugfix: Sharp raises min. damage for everything below the attribute value
# -------------
# TODO Schadensreduktion (Slider)
# TODO Plot Durchschnittsschaden ggü. verschiedenster SRs
# TODO Bugfix "subscript out of bounds" - Berechnung unterbinden
# TODO Waffenmerkmale: Exakt, Durchdringung, vielseitig
  # Vielseitige Nutzung entweder als Checkbox mitberücksichtigen und Werte
  # updaten, oder bei Checkbox beide Outputs (ein- und zweihändige Führung)
# TODO Waffenbeschreibung um Liste "Merkmale: Scharf 1, Kritisch 2, ..." ergänzen
# TODO Standardabweichungen
# TODO Ersetzen seq.int mit seq_along(x:y)
# TODO Erfolgsgrad-Bestimmungen - Wuchtangriff (Merkmal wuchtig)
  # Einfache Variante: Eingabe erwarteter Erfolgsgrade
  # Schwierige Variante: Fähigkeitswert + Wurfoption Wahrscheinlichkeiten bestimmen
  # und mit Wahrscheinlichkeiten der Schadensrechnung kombinieren 
  # (auch Fehlschläge in Durchschnittsschaden einfließen lassen etc.)
# TODO Gegnersimulation
# TODO Waffenauswahl (Presets?)
  # Abseits GitHub: Auswertungstabelle Mondstahlklingen mit allen Berechnungen
# TODO Trefferwahrscheinlichkeit
# TODO Vergleichsfunktion mehrere Plots / ineinander
# TODO Code kommentieren u. dokumentieren
# TODO README schreiben
# TODO Bonuswürfel (z.B. Pfeilzauber wie Scharfer Wind mit anderen Attributen)

# Fertig:
# TODO alle Waffenwürfel
# TODO Flat Bonus
# TODO Kumulative Wahrscheinlichkeiten (min. X Schaden, Funktionsparameter?)
# TODO Reverse kumulativ (max. X Schaden)


# Dice Calculations ----

calc_dice_avg <- function(sides, n) {
  ((sides + 1) / 2) * n
}

create_prob_vec <-
  function(n_sides,
           att_exact = 0,
           att_sharp = 0,
           att_critical = 0) {

    att_exact <- att_exact + 1
    
    seq <-
      c(seq.int(1, n_sides - 1), n_sides + att_critical)
    seq[seq %in% seq_len(att_sharp)] <- att_sharp
    # p <- 1 / n_sides
    # p <- seq.int(from = 1, to = n_sides * 2 - 1, by = ) * (1 / n_sides^2)
    
    p <- 1:n_sides
    p <- (p^att_exact - (p - 1)^att_exact) / n_sides^att_exact
    
    die <- data.table(eyes = seq, prob = p)
    die <- die[order(eyes), .(prob = sum(prob)), by = eyes]
    
    zeros <-
      data.table(eyes = seq.int(0, max(die[, eyes])), prob = rep(0))
    die <- rbind(die, zeros[!die, on = "eyes"])
    setorder(die)
    
    return(die[, prob])
  }

convolve_vecs <- function(n_d6 = 0,
                          n_d10 = 0,
                          att_exact = 0,
                          att_sharp = 0,
                          att_critical = 0) {
  if (n_d6 > 0) {
    p6s <-
      create_prob_vec(6, att_exact = att_exact, att_sharp = att_sharp, att_critical = att_critical)
  } else {
    p6s <- NA
  }
  
  if (n_d10 > 0) {
    p10s <-
      create_prob_vec(10, att_exact = att_exact, att_sharp = att_sharp, att_critical = att_critical)
  } else {
    p10s <- NA
  }
  
  vecs <- c(rep(list(p6s), n_d6), rep(list(p10s), n_d10))
  
  probs <- 1
  
  for (i in 1:length(vecs)) {
    probs <- convolve(probs, rev(vecs[[i]]), type = "open")
  }
  
  probs <- round(zapsmall(probs), 5)
  return(probs)
}

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

# Plots ----

create_prob_table <- function(n_d6 = 0,
                              n_d10 = 0,
                              flat_mod = 0,
                              att_exact = 0,
                              att_sharp = 0,
                              att_critical = 0) {
  
  probability <-
    convolve_vecs(
      n_d6 = n_d6,
      n_d10 = n_d10,
      att_exact = att_exact,
      att_sharp = att_sharp,
      att_critical = att_critical
    )

  prob_table <-
    data.table(damage = seq.int(0, length(probability) - 1), probability)
  prob_table[, damage := damage + flat_mod]
  prob_table[damage < 0, damage := 0]
  
  prob_table[, cum_prob_min := rev(cumsum(rev(probability)))]
  prob_table[damage == 0, cum_prob_min := first(cum_prob_min) / .N]
  
  prob_table[, cum_prob_max := cumsum(probability)]
  prob_table[damage == 0, cum_prob_max := last(cum_prob_max) / .N]
  
  min <- prob_table[probability != 0, min(damage)]
  max <- prob_table[probability != 0, max(damage)]
  
  prob_table <- prob_table[damage %between% c(min, max)]
  return(prob_table)
}
