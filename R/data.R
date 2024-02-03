# copy Hadley example https://r-pkgs.org/data.html

#' Cross-situational word learning models
#'
#' A collection of cross-situational word learning models that learn
#' trial-by-trial.
#'
#' @format ## `models`
#' A list of
#' \describe{
#'   \item{model}{baseline}
#'   \item{model}{Bayesian_decay}
#'   \item{model}{decay}
#'   \item{model}{fazly}
#'   \item{model}{fazlyt}
#'   \item{model}{kachergis}
#'   \item{model}{novelty}
#'   \item{model}{rescorla-wagner}
#'   \item{model}{guess-and-test}
#'   \item{model}{kachergis_sampling}
#'   \item{model}{multi_sampling}
#'   \item{model}{propose-but-verify}
#'   \item{model}{pursuit}
#'   \item{model}{strength}
#'   \item{model}{tilles}
#'   \item{model}{uncertainty}
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"models"


#' Cross-situational word learning experiment data
#'
#' A collection of data from human cross-situational word learning experiments,
#' including the trial-by-trial design of each experiment and the item-level
#' proportion of correct responses from a sample of N participants.
#'
#' @format ## `combined_data`
#' A list of 44 experimental conditions with trial orders and human performance:
#' \describe{
#'   \item{condition}{3x4}
#'   \item{condition}{3x4 1/.5}
#'   \item{condition}{3x4 1/.66}
#'   \item{condition}{3x4 +6o}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"combined_data"
