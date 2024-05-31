#' Create Animated Plot of Co-Occurrence Matrix from Training Trials
#'
#' This function takes a sequence of training trials, each consisting of words
#' and objects, and creates an animated plot of the word-object co-occurrence
#' matrix. The plot evolves over each trial, visually representing the
#' accumulation of co-occurrences. The animation is saved to a file if a
#' filename is provided.
#'
#' @param train A list representing the training data, where each element is a
#'   trial containing lists of words (`words`) and objects (`objects`).
#' @param filename A string specifying the filename where the animation should
#'   be saved. If NULL, no file will be saved.
#'
#' @return A long-format data frame where each row corresponds to a trial and
#'   includes columns for trial number, word, object, and the count of
#'   co-occurrences up to that trial.
#' @export
#'
#' @examples
#' plot_training_trials(xsl_datasets[[1]]$train)
plot_training_trials <- function(train, filename = NULL) {
  words <- sort(unique(unlist(train$words)))
  objects <- sort(unique(unlist(train$objects)))

  matrix_to_long <- function(m, t) {
    m |>
      as_tibble() |>
      mutate(word = rownames(m), .before = everything()) |>
      pivot_longer(-.data$word, names_to = "object", values_to = "coocs") |>
      mutate(trial = t)
  }

  m <- matrix(0, nrow = length(words), ncol = length(objects),
              dimnames = list(words, objects))
  df <- matrix_to_long(m, 0)

  for (t in seq_along(train$words)) {
    tr_w <- train$words[[t]]
    tr_o <- train$objects[[t]]
    m[tr_w, tr_o] = m[tr_w, tr_o] + 1
    df_t <- matrix_to_long(m, t)
    df <- bind_rows(df, df_t)
  }

  p <- ggplot(df, aes(x = .data$object, y = .data$word, fill = .data$coocs)) +
    geom_tile(color = "white", linewidth = 0.1) +
    viridis::scale_fill_viridis(name = "Count", option = "magma") +
    coord_equal() +
    labs(x = "Object", y = "Word") +
    gganimate::transition_states(.data$trial)
  print(p)

  if (!is.null(filename)) {
    gganimate::animate(p, format = "mp4",
                       renderer = gganimate::ffmpeg_renderer(format = "mp4"))
    gganimate::anim_save(filename)
  }

  as_tibble(df)
}

