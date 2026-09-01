# Rational Speech Act (RSA) recursion over a word-object meaning matrix.
#
# Pure-R port of wordlearn/rsa.py::rsa() (github.com/mcfrank/wurwur), used by
# predict_referent(pragmatic = TRUE). A speaker chooses words to be informative
# to a listener who reasons about that speaker; the recursion is what produces
# *strong* mutual exclusivity -- "the speaker could have used another word".
#
# This operates on any (word x object) matrix: entries are read as literal
# meanings (1 = the word can denote the object), and real values in (0, 1] work
# too. It is not specific to fgt2009 -- note that fgt2009's *corpus* scorer uses
# a different recursion convention (uniform-prior L0, one fewer iteration); this
# is deliberately the predict_referent / Smith et al. (2013) version.

.rsa_norm_rows <- function(x) x / rowSums(x)
.rsa_norm_cols <- function(x) sweep(x, 2, colSums(x), "/")

# Run the RSA recursion. Returns list(listener, speaker), each a (W, O) matrix.
#
# lex   : (W, O) literal meaning matrix.
# prior : (O,) prior over the referent. Objects with prior 0 are dropped from
#         the reasoning entirely and assigned listener probability 0.
# alpha : speaker rationality (softmax temperature).
# cost  : (W,) optional per-word production cost.
# depth : pragmatic recursions beyond L0/S1. depth = 0 gives the (prior-
#         weighted) literal listener L0 and speaker S1; depth = 1 the usual
#         one-step pragmatic listener/speaker.
xsl_rsa <- function(lex, prior, alpha = 3, cost = NULL, depth = 1) {
  W <- nrow(lex)
  O <- ncol(lex)
  if (is.null(cost)) cost <- rep(0, W)

  keep <- prior > 0
  lx <- lex[, keep, drop = FALSE]
  pr <- prior[keep] / sum(prior[keep])
  PR <- matrix(pr, W, sum(keep), byrow = TRUE)
  CO <- matrix(cost, W, sum(keep))

  suppressWarnings({
    L <- .rsa_norm_rows(lx * PR)                        # L0(object | word)
    S <- .rsa_norm_cols(exp(alpha * (log(L) - CO)))     # S1(word | object)
    for (i in seq_len(depth)) {
      L <- .rsa_norm_rows(S * PR)
      S <- .rsa_norm_cols(exp(alpha * (log(L) - CO)))
    }
  })

  listener <- matrix(0, W, O)
  listener[, keep] <- L
  speaker <- matrix(0, W, O)
  S[!is.finite(S)] <- 0                                 # unreachable objects -> 0
  speaker[, keep] <- S
  list(listener = listener, speaker = speaker)
}

#' Rational Speech Act pragmatic listener and speaker
#'
#' A small, self-contained RSA recursion over a word-by-object meaning matrix
#' (Frank & Goodman, 2012; Smith, Goodman & Frank, 2013), used by
#' [predict_referent()] with `pragmatic = TRUE`. `rsa_listener()` returns
#' `P(object | word)` for a listener who reasons about an informative speaker;
#' `rsa_speaker()` returns `P(word | object)` for that speaker. Both operate on
#' any non-negative `(word x object)` matrix, reading entries as literal
#' meaning weights.
#'
#' @param lex A `(word x object)` matrix of literal meaning weights.
#' @param prior Numeric prior over objects (one per column of `lex`). Objects
#'   with prior 0 are dropped from the reasoning and get listener probability 0.
#' @param ... Further arguments to the recursion: `alpha` (speaker rationality
#'   / softmax temperature, default 3), `cost` (optional per-word production
#'   cost vector), and `depth` (pragmatic recursions beyond the literal level;
#'   `0` gives the prior-weighted literal listener, `1` the usual one-step
#'   pragmatic one).
#'
#' @return `rsa_listener()`: a `(word x object)` matrix of `P(object | word)`.
#'   `rsa_speaker()`: a `(word x object)` matrix of `P(word | object)`.
#' @export
#'
#' @examples
#' # "wug" names either object; a pragmatic listener resolves it to the second
#' lex <- matrix(c(1, 0, 1, 1), 2, byrow = TRUE)
#' rsa_listener(lex, c(0.5, 0.5), alpha = 3, depth = 1)
rsa_listener <- function(lex, prior, ...) xsl_rsa(lex, prior, ...)$listener

#' @rdname rsa_listener
#' @export
rsa_speaker <- function(lex, prior, ...) xsl_rsa(lex, prior, ...)$speaker
