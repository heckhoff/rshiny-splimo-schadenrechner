# Test basic functionality with only d6
test_that("calculate_probabilities() works for basic cases with only d6", {
  dt_only_d6 <- readRDS(test_path("fixtures", "only_d6.RDS"))
  result <- calculate_probabilities(n_d6 = 1)
  expect_equal(round(result, 3), dt_only_d6)
})

# Test basic functionality with only d10
test_that("calculate_probabilities() works for basic cases with only d10", {
  dt_only_d10 <- readRDS(test_path("fixtures", "only_d10.RDS"))
  result <- calculate_probabilities(n_d10 = 1)
  expect_equal(round(result, 3), dt_only_d10)
})


# Test flat modifier
test_that("calculate_probabilities() works with flat_mod", {
  dt_flat_mod <- readRDS(test_path("fixtures", "flat_mod.RDS"))
  result <- calculate_probabilities(n_d10 = 1, flat_mod = 3)
  expect_equal(round(result, 3), dt_flat_mod)
})

# Test critical attribute
test_that("calculate_probabilities() works with the critical attribute", {
  dt_att_critical <-
    readRDS(test_path("fixtures", "att_critical.RDS"))
  result <- calculate_probabilities(n_d10 = 1, att_critical = 3)
  expect_equal(round(result, 3), dt_att_critical)
})

# Test exact attribute
test_that("calculate_probabilities() works with the exact attribute", {
  dt_att_exact <- readRDS(test_path("fixtures", "att_exact.RDS"))
  result <- calculate_probabilities(n_d10 = 1, att_exact = 3)
  expect_equal(round(result, 3), dt_att_exact)
})

# Test massive attribute
test_that("calculate_probabilities() works with the massive attribute", {
  dt_att_massive <- readRDS(test_path("fixtures", "att_massive.RDS"))
  result <-
    calculate_probabilities(
      n_d10 = 1,
      att_massive = TRUE,
      success_level = 3
    )
  expect_equal(round(result, 3), dt_att_massive)
})

# Test penetration attribute
test_that("calculate_probabilities() works with the penetration attribute", {
  dt_att_penetration <-
    readRDS(test_path("fixtures", "att_penetration.RDS"))
  result <-
    calculate_probabilities(
      n_d10 = 1,
      att_penetration = 3,
      damage_reduction = 4
    )
  expect_equal(round(result, 3), dt_att_penetration)
})

# Test sharp attribute
test_that("calculate_probabilities() works with the sharp attribute", {
  dt_att_sharp <- readRDS(test_path("fixtures", "att_sharp.RDS"))
  result <- calculate_probabilities(n_d10 = 1, att_sharp = 3)
  expect_equal(round(result, 3), dt_att_sharp)
})

# Test versatile attribute
test_that("calculate_probabilities() works with the versatile attribute", {
  dt_att_versatile <-
    readRDS(test_path("fixtures", "att_versatile.RDS"))
  result <- calculate_probabilities(n_d10 = 1, att_versatile = TRUE)
  expect_equal(round(result, 3), dt_att_versatile)
})


# Test complex cases with various arguments together
test_that("calculate_probabilities() works with all attributes together", {
  dt_no_args <- readRDS(test_path("fixtures", "no_arguments.RDS"))
  dt_some_args <-
    readRDS(test_path("fixtures", "some_arguments.RDS"))
  dt_all_args <- readRDS(test_path("fixtures", "all_arguments.RDS"))
  dt_all_args_d6 <-
    readRDS(test_path("fixtures", "all_arguments_d6.RDS"))
  dt_all_args_d10 <-
    readRDS(test_path("fixtures", "all_arguments_d10.RDS"))
  dt_edge_case_negative <-
    readRDS(test_path("fixtures", "edge_case_negative.RDS"))

  result_1 <-
    calculate_probabilities(
      n_d6 = 0,
      n_d10 = 0,
      flat_mod = 0,
      att_exact = 0,
      att_sharp = 0,
      att_critical = 0,
      att_penetration = 0,
      att_massive = FALSE,
      att_versatile = FALSE,
      damage_reduction = 0,
      success_level = 0
    )
  expect_equal(round(result_1, 3), dt_no_args)

  result_2 <-
    calculate_probabilities(
      n_d6 = 0,
      n_d10 = 2,
      flat_mod = -3,
      att_exact = 1,
      att_sharp = 3,
      att_critical = 4,
      att_penetration = 0,
      att_massive = FALSE,
      att_versatile = FALSE,
      damage_reduction = 0,
      success_level = 0
    )
  expect_equal(round(result_2, 3), dt_some_args)

  result_3 <-
    calculate_probabilities(
      n_d6 = 1,
      n_d10 = 2,
      flat_mod = -3,
      att_exact = 1,
      att_sharp = 1,
      att_critical = 3,
      att_penetration = 2,
      att_massive = TRUE,
      att_versatile = TRUE,
      damage_reduction = 3,
      success_level = 1
    )
  expect_equal(round(result_3, 3), dt_all_args)

  result_4 <-
    calculate_probabilities(
      n_d6 = 2,
      n_d10 = 0,
      flat_mod = -3,
      att_exact = 1,
      att_sharp = 1,
      att_critical = 3,
      att_penetration = 2,
      att_massive = TRUE,
      att_versatile = TRUE,
      damage_reduction = 3,
      success_level = 1
    )
  expect_equal(round(result_4, 3), dt_all_args_d6)

  result_5 <-
    calculate_probabilities(
      n_d6 = 0,
      n_d10 = 2,
      flat_mod = -3,
      att_exact = 1,
      att_sharp = 1,
      att_critical = 3,
      att_penetration = 2,
      att_massive = TRUE,
      att_versatile = TRUE,
      damage_reduction = 3,
      success_level = 1
    )
  expect_equal(round(result_5, 3), dt_all_args_d10)

  result_6 <-
    calculate_probabilities(
      n_d6 = 1,
      n_d10 = 0,
      flat_mod = -14,
      att_exact = 3,
      att_sharp = 5,
      att_critical = 5,
      att_penetration = 2,
      att_massive = TRUE,
      att_versatile = TRUE,
      damage_reduction = 2,
      success_level = 1
    )
  expect_equal(round(result_6, 3), dt_edge_case_negative)
})
