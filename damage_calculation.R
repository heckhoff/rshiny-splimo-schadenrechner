library(ggplot2)
library(data.table)

options(scipen = 999)

# https://anydice.com/
# https://www.dungeonsolvers.com/2018/03/29/average-dice-roll/
# https://stats.stackexchange.com/questions/177163/what-is-the-distribution-for-various-polyhedral-dice-all-rolled-at-once

# TODO alle Waffenwürfel
# TODO Flat Bonus
# TODO Schadensreduktion (Slider)
# TODO Waffenmerkmale: Kritisch, Scharf, Exakt
# TODO Mondzeichen
# TODO Standardabweichungen
# TODO Kumulative Wahrscheinlichkeiten (min. X Schaden, Funktionsparameter?)
# TODO Reverse kumulativ (max. X Schaden)
# TODO Waffengeschwindigkeit
# TODO Gegnersimulation
# TODO Waffenauswahl (Presets?)
# TODO Trefferwahrscheinlichkeit
# TODO Vergleichsfunktion mehrere Plots / ineinander

# Dice Calculations ----

calc_dice_avg <- function(sides, n) {
  ((sides + 1) / 2) * n
}

calc_mean_damage <- function(n_6s = 0,
                             n_10s = 0,
                             flat_mod = 0) {
  avg_roll_6s <- calc_dice_avg(6, n_6s)
  avg_roll_10s <- calc_dice_avg(10, n_10s)
  mean_damage <- (avg_roll_6s + avg_roll_10s + flat_mod)
  return(mean_damage)
}

convolve_vecs <- function(n_6s = 0,
                          n_10s = 0) {
  p6s <- c(0, rep(1 / 6, 6))
  p10s <- c(0, rep(1 / 10, 10))
  
  vecs <- c(rep(list(p6s), n_6s), rep(list(p10s), n_10s))
  
  probs <- 1
  
  for (i in 1:length(vecs)) {
    probs <- convolve(probs, rev(vecs[[i]]), type = "open")
  }
  
  probs <- round(zapsmall(probs), 5)
  return(probs)
}

create_weapon_txt <- function(n_6s = 0,
                              n_10s = 0,
                              flat_mod = 0) {
  fcase(
    n_6s != 0 & n_10s == 0,
    paste0(n_6s, "W6", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    }),
    n_6s == 0 & n_10s != 0,
    paste0(n_10s, "W10", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    }),
    default = paste0(n_6s, "W6+", n_10s, "W10", if (flat_mod != 0) {
      paste0(symnum(flat_mod, c(-Inf, 0, Inf), c("", "+")), flat_mod)
    })
  )
}

# Plots ----

create_prob_table <- function(n_6s = 0,
                              n_10s = 0,
                              flat_mod = 0) {
  
  n_dice <- n_6s + n_10s
  min_damage <- n_6s + n_10s + flat_mod
  max_damage <- (n_6s * 6) + (n_10s * 10) + flat_mod
  
  # mean_damage <-
  #   calc_mean_damage(n_6s = n_6s,
  #                    n_10s = n_10s,
  #                    flat_mod = flat_mod)
  
  probability <-
    convolve_vecs(n_6s = n_6s, n_10s = n_10s)[-(1:n_dice)]
  
  prob_table <-
    data.table(damage = seq.int(min_damage, max_damage), probability)
  prob_table[damage < 0, damage := 0]
  
  prob_table[, cum_prob_min := rev(cumsum(rev(probability)))]
  prob_table[damage == 0, cum_prob_min := first(cum_prob_min) / .N]
  
  prob_table[, cum_prob_max := cumsum(probability)]
  prob_table[damage == 0, cum_prob_max := last(cum_prob_max) / .N]
  return(prob_table)
}