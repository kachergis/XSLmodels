test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("basic model creation works", {
  # Test baseline model
  mod <- baseline()
  expect_s3_class(mod, "xslMod")
  expect_equal(mod$name, "baseline")
  
  # Test decay model
  mod <- decay(C = 0.98)
  expect_s3_class(mod, "xslMod")
  expect_equal(mod$name, "decay")
  expect_equal(mod$params$C, 0.98)
})

test_that("xslData creation works", {
  data <- xslData(
    train = list(words = list(c(1, 2), c(2, 3)),
                 objects = list(c(1, 2), c(2, 3))),
    test = list(words = list(1, 2, 3),
                objects = list(1:3, 1:3, 1:3)),
    label = "test dataset",
    condition = "test condition"
  )
  
  expect_s3_class(data, "xslData")
  expect_equal(data$label, "test dataset")
  expect_equal(length(data$train$words), 2)
})

test_that("helper functions work", {
  # Test show_models
  models <- show_models()
  expect_type(models, "character")
  expect_true(length(models) > 0)
  expect_true("baseline" %in% models)
  
  # Test show_datasets (requires data to be loaded)
  if (exists("xsl_datasets")) {
    datasets <- show_datasets()
    expect_s3_class(datasets, "data.frame")
    expect_true(nrow(datasets) > 0)
  }
})

test_that("co-occurrence matrix creation works", {
  train_data <- list(
    words = list(c(1, 2), c(2, 3), c(1, 3)),
    objects = list(c(1, 2), c(2, 3), c(1, 3))
  )
  
  cooc_matrix <- create_cooc_matrix(train_data)
  expect_true(is.matrix(cooc_matrix))
  expect_equal(dim(cooc_matrix), c(3, 3))
  # each trial tallies every word x every object present (2x2 = 4 cells per trial)
  expect_equal(sum(cooc_matrix), 12)
})

test_that("model running works with example data", {
  # Test with example ambiguous condition
  ex_data <- get_example_ambiguous_condition()
  mod <- baseline()
  
  result <- xsl_run(mod, ex_data)
  expect_type(result$sse, "double")
  expect_true(length(result$fits) > 0)
})
