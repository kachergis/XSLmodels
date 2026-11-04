train <- list(words = list(c(1, 2), c(2, 3), NA, c(1, 3)),
              objects = list(c(1, 2), c(2, 3), NA, c(1, 2)))
final <- function(df) {
  last <- df[df$trial == max(df$trial), ]
  tapply(last$coocs, list(word = last$word, object = last$object), identity)
}

test_that("training_cooccurrences() accumulates co-occurrence counts trial by trial", {
  df <- training_cooccurrences(train)
  expect_named(df, c("word", "object", "coocs", "trial"))
  expect_equal(sort(unique(df$trial)), 0:4)
  expect_equal(nrow(df), 5 * 3 * 3)
  expect_true(all(df$coocs[df$trial == 0] == 0))
  # an empty trial repeats the previous counts
  expect_equal(df$coocs[df$trial == 3], df$coocs[df$trial == 2])
  # counts never decrease
  wide <- tidyr::pivot_wider(df, names_from = "trial", values_from = "coocs")
  expect_true(all(apply(as.matrix(wide[, -(1:2)]), 1, \(x) all(diff(x) >= 0))))
  # the final counts are every word x object pairing over all trials
  expected <- matrix(c(2, 2, 0,
                       1, 2, 1,
                       1, 2, 1), 3, byrow = TRUE)
  expect_equal(unname(final(df)), expected)
})

test_that("training_cooccurrences() indexes by label, not position (#14)", {
  # labels that aren't 1..n: positional indexing would put the counts in the
  # wrong cells, or fail
  tr <- list(words = list(c(10, 30), 30), objects = list(c("b", "c"), "c"))
  f <- final(training_cooccurrences(tr))
  expect_equal(dimnames(f), list(word = c("10", "30"), object = c("b", "c")))
  expect_equal(unname(f), matrix(c(1, 1, 1, 2), 2, byrow = TRUE))
})

test_that("training_cooccurrences() totals match a real condition's training", {
  d <- xsl_datasets[[1]]
  f <- final(training_cooccurrences(d$train))
  pairs <- sum(mapply(\(w, o) length(w) * length(o), d$train$words, d$train$objects))
  expect_equal(sum(f), pairs)
})

test_that("cooccurrence_animation() builds one animation state per trial", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("viridis")
  df <- training_cooccurrences(train)
  p <- cooccurrence_animation(df)
  expect_s3_class(p, "gganim")
  expect_identical(p$data, df)
  expect_s3_class(p$layers[[1]]$geom, "GeomTile")
})

test_that("plot_training_trials() explains which packages it needs", {
  local_mocked_bindings(has_animation_packages = function() FALSE)
  expect_error(plot_training_trials(train), "viridis.*gganimate")
})

test_that("plot_training_trials() renders and returns the co-occurrence data", {
  skip_on_cran()
  skip_if_not_installed("gganimate")
  skip_if_not_installed("viridis")
  # printing would render every frame; stub it, and check the animation is
  # what gets shown and that the working directory is restored
  printed <- NULL
  local_mocked_bindings(show_animation = function(p) printed <<- p)
  wd <- getwd()
  df <- plot_training_trials(train)
  expect_equal(df, training_cooccurrences(train))
  expect_s3_class(printed, "gganim")
  expect_equal(getwd(), wd)
})
