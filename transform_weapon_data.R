library(data.table)
library(openxlsx)
library(rex)
library(stringr)

# Import ----

msk <-
  setDT(read.xlsx("raw_waffenliste_mondstahlklingen.xlsx", sheet = 1))
msk[, id := .I]
setcolorder(msk, neworder = "id")

# Dice ----

vec_n_d6 <-
  msk[grepl("W6", damage), sub("*W6.*", "", damage), by = id]
vec_n_d10 <-
  msk[grepl("W10", damage), sub("*W10.*", "", damage), by = id]
vec_flat_pos <-
  msk[grepl("\\+", damage), sub(".*\\+", "", damage), by = id]
vec_flat_neg <-
  msk[grepl("-", damage), sub(".*\\-", "-", damage), by = id]

msk[vec_n_d6, n_d6 := V1, on = "id"]
msk[vec_n_d10, n_d10 := V1, on = "id"]
msk[vec_flat_pos, flat_mod := V1, on = "id"]
msk[vec_flat_neg, flat_mod := V1, on = "id"]

# Attributes ----

msk[attributes != "–", n_attributes := str_count(attributes, ",") + 1]

capture_stufe <- function(string) {
  rex(paste(string, "("),
      capture(except_any_of(")")),
      ")")
}

msk[grepl("Ablenkend", attributes),
    ablenkend := re_matches(attributes, capture_stufe("Ablenkend")), by = id]

msk[grepl("Defensiv", attributes),
    defensiv := re_matches(attributes, capture_stufe("Defensiv")), by = id]

msk[grepl("Doppelwaffe", attributes), doppelwaffe := 1, by = id]

msk[grepl("Durchdringung", attributes),
    durchdringung := re_matches(attributes, capture_stufe("Durchdringung")), by = id]

msk[grepl("Entwaffnend", attributes),
    entwaffnend := re_matches(attributes, capture_stufe("Entwaffnend")), by = id]

msk[grepl("Entwaffnungsimmunität", attributes), entwaffnungsimmunitaet := 1, by = id]

msk[grepl("Entwaffnungsschutz", attributes),
    entwaffnungsschutz := re_matches(attributes, capture_stufe("Entwaffnungsschutz")), by = id]

msk[grepl("Exakt", attributes),
    exakt := re_matches(attributes, capture_stufe("Exakt")), by = id]

msk[grepl("Ferndistanz", attributes), ferndistanz := 1, by = id]

msk[grepl("Freihändig", attributes), freihaendig := 1, by = id]

msk[grepl("Improvisiert", attributes), improvisiert := 1, by = id]

msk[grepl("Kletterhilfe", attributes), kletterhilfe := 1, by = id]

msk[grepl("Kritisch", attributes),
    kritisch := re_matches(attributes, capture_stufe("Kritisch")), by = id]

msk[grepl("Lange Waffe", attributes), lange_waffe := 1, by = id]

msk[grepl("Nahkampftauglich", attributes), nahkampftauglich := 1, by = id]

msk[grepl("Paarwaffe", attributes), paarwaffe := 1, by = id]

msk[grepl("Parierwaffe", attributes), parierwaffe := 1, by = id]

msk[grepl("Primitiv", attributes), primitiv := 1, by = id]

msk[grepl("Reiterwaffe", attributes),
    reiterwaffe := re_matches(attributes, capture_stufe("Reiterwaffe")), by = id]

msk[grepl("Rückkehrend", attributes), rueckkehrend := 1, by = id]

msk[grepl("Scharf", attributes),
    scharf := re_matches(attributes, capture_stufe("Scharf")), by = id]

msk[grepl("Stumpf", attributes), stumpf := 1, by = id]

msk[grepl("Teilbar", attributes), teilbar := 1, by = id]

msk[grepl("Treffsicher", attributes), treffsicher := 1, by = id]

msk[grepl("Umklammern", attributes), umklammern := 1, by = id]

msk[grepl("Unhandlich", attributes), unhandlich := 1, by = id]

msk[grepl("Unauffällig", attributes), unauffaellig := 1, by = id]

msk[grepl("Vielseitig", attributes), vielseitig := 1, by = id]

msk[grepl("Wuchtig", attributes), wuchtig := 1, by = id]

msk[grepl("Wurffähig", attributes), wurffaehig := 1, by = id]

msk[grepl("Zweihändig", attributes), zweihaendig := 1, by = id]

msk[, speed := as.numeric(speed)]
msk[, 18:47] <- msk[, lapply(.SD, as.numeric), .SDcols = 18:47]
msk[is.na(msk)] <- 0

# Correct Ranged Weapons ----

msk[(type == "Schusswaffen" | type == "Wurfwaffen") & grepl("Nahkampftauglich", attributes), name := paste(name, "(Fernkampf)")]
# Addition is necessary for correcting the damage per tick in shiny
msk[type == "Schusswaffen" | type == "Wurfwaffen", speed := speed + 3]

# Check validity of scraping ----

msk_val_check <- rowSums(msk[, replace(.SD, .SD > 0, 1), .SDcols = 18:47])
msk[, val_check := msk_val_check]
msk[n_attributes != val_check]
msk[, val_check := NULL]

setorderv(msk, cols = "name")

col_vec <- c(20, 23, 26, 27, 28, 29, 31, 32, 33, 34, 35, 37, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)
msk[, (col_vec)] <- msk[, lapply(.SD, as.logical), .SDcols = (col_vec)]

write.xlsx(msk, "waffenliste_mondstahlklingen.xlsx")
