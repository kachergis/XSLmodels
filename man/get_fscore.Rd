% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitting.R
\name{get_fscore}
\alias{get_fscore}
\title{Calculate F-score, Precision, Recall, and Specificity for a Knowledge Matrix at a Given Threshold}
\usage{
get_fscore(thresh, mat, fscore_only = T, gold_lexicon = c(), verbose = F)
}
\arguments{
\item{thresh}{A numeric value representing the threshold for considering an association between a word and a referent as positive.}

\item{mat}{A matrix representing the knowledge matrix with words as rows and referents as columns.}

\item{fscore_only}{Logical; if TRUE, only the F-score is returned. If FALSE, a data frame with precision, recall, specificity, and F-score for each threshold is returned.}

\item{gold_lexicon}{Optional; a data frame or list where each row/element represents a word-object pair in the gold lexicon. If provided, it is used to calculate true positives, false positives, and false negatives.}

\item{verbose}{Logical; if TRUE, additional details about true positives, false positives, and false negatives are printed.}
}
\value{
If \code{fscore_only} is TRUE, returns a single numeric value representing the F-score. If \code{fscore_only} is FALSE, returns a tibble (data frame) with columns for threshold, precision, recall, specificity, and F-score at each threshold.
}
\description{
This function calculates the F-score, precision, recall, and specificity for a given knowledge matrix at a specified threshold. It uses the concept of true positives, false positives, and false negatives, determined from the knowledge matrix and an optional gold lexicon. The function is useful for evaluating the performance of a model in terms of its ability to correctly identify associations between words and referents.
}
