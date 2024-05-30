#' Get example ambiguous condition
#'
#' This function creates an example condition with two trials, and two
#' word-object pairs per trial. Each word-object pair appears only once, and
#' thus each word has a 50% chance of being associated with the correct
#' referent. The function returns a list of the trials with nested words and
#' objects per trial, as well as a vector of the conditional probability of
#' correctly selecting the correct referent given each word.
#'
#' @return An object of class `xslData` containing a list of training trials
#'   (with nested words and objects per trial) and accuracy (P(correct referent
#'   | word)).
#' @export
#'
#' @examples
#' get_example_ambiguous_condition()
get_example_ambiguous_condition <- function() {
  xslData(train = list(words = list(c(1, 2), c(3, 4)),
                       objects = list(c(2, 1), c(3, 4))),
          accuracy = rep(0.5, 4),
          label = "example condition",
          condition = "ambiguous")
  # each word has 50% chance of being associated with correct referent
}


#' Get example unambiguous condition
#'
#' This function creates an example condition with three trials and two
#' word-object pairs per trial. Each word-object pair appears twice, and so if
#' the most frequently co-occurring referent for each word is selected then all
#' three words would be learned.
#'
#' @return An object of class `xslData` containing a list of training trials
#'   (with nested words and objects per trial) and accuracy (P(correct referent
#'   | word)).
#' @export
#'
#' @examples
#' get_example_unambiguous_condition()
get_example_unambiguous_condition <- function() {
  xslData(train = list(words = list(c(1, 2), c(1, 3), c(3, 2)),
                       objects = list(c(2, 1), c(1, 3), c(2, 3))),
          accuracy = rep(0.5, 3),
          label = "example condition",
          condition = "unambiguous")
  # each word has 50% chance of being associated with correct referent
}
