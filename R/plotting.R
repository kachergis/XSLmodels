
#' Create Animated Plot of Co-Occurrence Matrix from Training Trials
#'
#' This function takes a sequence of training trials, each consisting of words
#' and objects, and creates an animated plot of the word-object co-occurrence matrix.
#' The plot evolves over each trial, visually representing the accumulation of
#' co-occurrences. The animation is saved to a file if a filename is provided.
#'
#' @param trials A list representing the training data, where each element is a
#' trial containing lists of words (`words`) and objects (`objs`).
#' @param fname A string specifying the filename where the animation should be saved.
#' If left as an empty string (''), no file will be saved.
#'
#' @return A long-format data frame where each row corresponds to a trial and
#' includes columns for trial number, word, object, and the count of co-occurrences
#' up to that trial.
#' @export
plot_training_trials <- function(trials, fname='') {
  # need to translate to long format: trial, word, object, coocs
  words = sort(unique(unlist(trials$words)))
  objs = sort(unique(unlist(trials$objs)))
  #df <- dplyr::tibble()
  df <- expand.grid(trial = 0, word = words, object = objs, coocs = 0)
  M <- matrix(0, nrow=length(words), ncol=length(objs))
  rownames(M) = sort(unique(unlist(trials$words)))
  colnames(M) = sort(unique(unlist(trials$objs)))
  #coocs <- rep(0, length(words))
  for(t in 1:length(trials$words)) {
    tr_w <- trials$words[[t]]
    tr_o <- trials$objs[[t]]
    #create_cooc_matrix()
    M[tr_w, tr_o] = M[tr_w, tr_o] + 1
    # probably wise to replace this with pivot_wider/spread
    tmp <- dplyr::bind_cols(expand.grid(word = words, object = objs),
                            coocs = as.vector(M)) # as.vector(M) is by column
    tmp$trial = t
    df <- dplyr::bind_rows(df, tmp)
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x=object, y=word, fill = coocs)) +
    ggplot2::geom_tile(color='White', linewidth=0.1) + #
    viridis::scale_fill_viridis(name="Count", option="magma") +
    ggplot2::coord_equal() + ggplot2::theme_classic() +
    ggplot2::xlab("Object") + ggplot2::ylab("Word") +
    #ggplot2::ggtitle(paste0("Trial ",trial))
    gganimate::transition_states(trial)
  #print(p)
  if(fname!='') gganimate::anim_save(fname, animation=p)
  # gifski::save_gif(p, fname)
  return(df)
}

