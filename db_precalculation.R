library(DBI)
library(RSQLite)
library(data.table)
library(doParallel)

cl <- makeCluster(3,
#   type = "PSOCK"
# )
type = "FORK") # only use if on Linux-system

registerDoParallel(cl)

source("damage_calculation.R")

# Create/Connect database ----
splimo_db <-
  dbConnect(RSQLite::SQLite(), "data/splimo_db.sqlite")

# Create table
# dbRemoveTable(splimo_db, "dice_combinations")

dbCreateTable(
  conn = splimo_db,
  name = "dice_combinations",
  fields = c(
    comb_index = "INTEGER",
    comb_id = "INTEGER",
    n_d6 = "INTEGER",
    n_d10 = "INTEGER",
    att_exact = "INTEGER",
    att_sharp = "INTEGER",
    att_critical = "INTEGER",
    combinations = "BLOB"
  )
)

# Define functions ----
create_argument_combinations <- function(d6,
                                         d10,
                                         exact,
                                         sharp,
                                         critical) {
  comb_table <- as.data.table(expand.grid(
    list(
      att_sharp = sharp,
      att_critical = critical,
      n_d6 = d6,
      n_d10 = d10,
      att_exact = exact
    )
  ))
  comb_table <-
    comb_table[n_d6 != 0L | n_d10 != 0L]
  return(comb_table)
}

calculate_argument_combination <-
  function(arguments, cluster) {
    calculation_res <- combine_dice(
      n_d6 = unlist(arguments["n_d6"]),
      n_d10 = unlist(arguments["n_d10"]),
      att_exact = unlist(arguments["att_exact"]),
      att_sharp = unlist(arguments["att_sharp"]),
      att_critical = unlist(arguments["att_critical"]),
      cluster = cluster
    )

    new_row <- data.table(
      comb_index = unlist(arguments["comb_index"]),
      comb_id = unlist(arguments["comb_id"]),
      n_d6 = unlist(arguments["n_d6"]),
      n_d10 = unlist(arguments["n_d10"]),
      att_exact = unlist(arguments["att_exact"]),
      att_sharp = unlist(arguments["att_sharp"]),
      att_critical = unlist(arguments["att_critical"]),
      combinations = I(list(
        serialize(object = calculation_res, connection = NULL)
      ))
    )
    new_row$combinations <-
      unclass(new_row$combinations)
    # combinations = blob::blob(serialize(
    #   object = calculation_res, connection = NULL
    # ))

    return(new_row)
  }

append_calculation <- function(calculation, db) {
  dbAppendTable(
    conn = db,
    name = "dice_combinations",
    value = calculation
  )
  return(TRUE)
}

# Apply over all possible argument combinations
argument_combs <- create_argument_combinations(
  d6 = 0L:5L,
  d10 = 0L,
  exact = 0L:3L,
  sharp = 0L:5L,
  critical = 0L:5L
)

# Index combinations
argument_combs[, comb_index := seq_len(nrow(argument_combs))]
argument_combs[, comb_id := do.call(paste0, .SD), .SDcols = c(
  "n_d6",
  "n_d10",
  "att_exact",
  "att_sharp",
  "att_critical"
)]
argument_combs[, comb_id := as.integer(comb_id)]


apply(
  X = argument_combs,
  MARGIN = 1,
  FUN = function(arguments, db, cluster) {
    print(paste("Starting calculation", unlist(arguments["comb_index"])))
    print(arguments)
    calculation <-
      calculate_argument_combination(arguments = arguments, cluster = cluster)
    append_calculation(calculation = calculation, db = db)
    rm(calculation)
    gc()
    print(paste("Finished calculation", unlist(arguments["comb_index"])))
  },
  db = splimo_db,
  cluster = cl
)

# test <- dbReadTable(splimo_db, "dice_combinations")
# unserialize(unlist(test$combinations[16]))
dbDisconnect(splimo_db)

stopCluster(cl)
