# Model comparison + entropy-ablation on the Kachergis (2012, CogSci)
# highlighting dataset (data-raw/add_kachergis2012_highlighting.R).
#
# Directly answers the CogSci reviewers' questions (see
# associative_word_learning/"reviews - domain-general CogSci 2012.rtf",
# Review 1): "What's the model competing against? ... How important is the
# entropy function?"
#
# Because the words-as-cues condition's ambiguous ("I") item can't be
# scored through xslData's standard accuracy/mafc_test convention (see the
# long comment in add_kachergis2012_highlighting.R), this script bypasses
# xsl_fit()'s default sse and instead fits directly against the four
# response proportions the paper actually reports (PE-E, PL-L, I-E, I-L),
# extracted from each model's returned association matrix -- the same
# quantities, and the same fitting target, the original paper used.

devtools::load_all("..")
library(purrr)

words_dat <- kachergis2012_highlighting[["words as cues"]]
objects_dat <- kachergis2012_highlighting[["objects as cues"]]

human_words <- c(PE_E = 0.69, PL_L = 0.82, I_E = 0.51, I_L = 0.25)
human_objects <- c(PE_E = 0.60, PL_L = 0.71, I_E = 0.28, I_L = 0.16)

# The paper's reported response probabilities are softmax choices over the
# association matrix (phi = discrimination), not raw row-normalized
# strengths -- so phi is fit as an extra free parameter and applied here,
# uniformly across every candidate model, before scoring.
softmax_mat <- function(m, phi) {
  e <- exp(phi * m)
  e / rowSums(e)
}

score_words_as_cues <- function(m, phi) {
  sm <- softmax_mat(m, phi)
  rn <- function(w) sm[as.character(w), ]
  c(PE_E = mean(c(rn(1)["1"], rn(4)["3"])),
    PL_L = mean(c(rn(2)["2"], rn(5)["4"])),
    I_E  = mean(c(rn(3)["1"], rn(6)["3"])),
    I_L  = mean(c(rn(3)["2"], rn(6)["4"])))
}

score_objects_as_cues <- function(m, phi) {
  sm <- softmax_mat(m, phi)
  rn <- function(w) sm[as.character(w), ]
  c(PE_E = mean(c(rn(1)["1"], rn(3)["4"])),
    PL_L = mean(c(rn(2)["2"], rn(4)["5"])),
    I_E  = mean(c(rn(1)["3"], rn(3)["6"])),
    I_L  = mean(c(rn(2)["3"], rn(4)["6"])))
}

r_squared <- function(pred, human) 1 - sum((pred - human) ^ 2) / sum((human - mean(human)) ^ 2)

# `par` is the model's own free parameters followed by phi (softmax) as the
# last element; `score_fn` takes (matrix, phi).
fit_model <- function(model, dat, score_fn, human, lower, upper, control = xslControl(),
                      deoptim_control = DEoptim::DEoptim.control(NP = 20, itermax = 20, trace = FALSE)) {
  n_model_par <- sum(vapply(model$params, is.numeric, logical(1)))
  objective <- function(par) {
    sse <- tryCatch({
      m <- update_params(model, par[seq_len(n_model_par)])
      phi <- par[n_model_par + 1]
      mat <- suppressWarnings(xsl_run(m, dat, control = control)$fits[[1]]$matrix)
      pred <- score_fn(mat, phi)
      sum((pred - human) ^ 2)
    }, error = function(e) NA_real_)
    if (is.na(sse)) Inf else sse
  }
  if (length(lower) == 0) {
    pred <- score_fn(suppressWarnings(xsl_run(model, dat, control = control)$fits[[1]]$matrix), 1)
    return(list(par = numeric(0), sse = sum((pred - human) ^ 2), pred = pred))
  }
  fit <- DEoptim::DEoptim(objective, lower = lower, upper = upper, control = deoptim_control)
  best <- fit$optim$bestmem
  m <- update_params(model, best[seq_len(n_model_par)])
  pred <- score_fn(suppressWarnings(xsl_run(m, dat, control = control)$fits[[1]]$matrix), best[n_model_par + 1])
  list(par = best, sse = fit$optim$bestval, pred = pred)
}

## ---- 1. Reproduce the paper's own reported fit (no re-fitting) -----------
## Paper's best-fitting parameters: words-as-cues chi=.11, lambda=.46,
## alpha=1, phi=6.16 (R2=.984); objects-as-cues chi=.12, lambda=.37, alpha=1,
## phi=6.16 (R2=.884).
cat("=== 1. Reproducing the paper's own reported fit (uncfam entropy) ===\n")
paper_words <- update_params(uncfam(X = .1, B = .1, C = 1, variant = "entropy"), c(.11, .46, 1))
pred_words <- score_words_as_cues(suppressWarnings(xsl_run(paper_words, words_dat)$fits[[1]]$matrix), 6.16)
cat("Words as cues -- predicted:\n"); print(round(pred_words, 3))
cat("Words as cues -- human:\n"); print(human_words)
cat(sprintf("R^2 = %.3f, SSE = %.4f\n\n", r_squared(pred_words, human_words), sum((pred_words - human_words)^2)))

paper_objects <- update_params(uncfam(X = .1, B = .1, C = 1, variant = "entropy"), c(.12, .37, 1))
pred_objects <- score_objects_as_cues(suppressWarnings(xsl_run(paper_objects, objects_dat)$fits[[1]]$matrix), 6.16)
cat("Objects as cues -- predicted:\n"); print(round(pred_objects, 3))
cat("Objects as cues -- human:\n"); print(human_objects)
cat(sprintf("R^2 = %.3f, SSE = %.4f\n\n", r_squared(pred_objects, human_objects), sum((pred_objects - human_objects)^2)))

## ---- 2. Entropy ablation: uncfam variants ---------------------------------
## Directly answers Review 1: "How important is the entropy function?" --
## compares the full model (entropy: competing uncertainty + familiarity)
## against a familiarity-only bias (novelty) and an uncertainty-only bias,
## refit from scratch against the same 4 targets for each condition.
cat("=== 2. Entropy ablation (uncfam variants) ===\n")
variants <- c("entropy", "novelty", "uncertainty-only")
ablation <- map(variants, function(v) {
  base <- uncfam(X = .1, B = .5, C = 1, variant = v)
  # X, B, C, phi
  fw <- fit_model(base, words_dat, score_words_as_cues, human_words,
                  lower = c(0, 0, .8, 0), upper = c(1, 10, 1, 20))
  fo <- fit_model(base, objects_dat, score_objects_as_cues, human_objects,
                  lower = c(0, 0, .8, 0), upper = c(1, 10, 1, 20))
  tibble::tibble(variant = v,
                sse_words = fw$sse, r2_words = r_squared(fw$pred, human_words),
                sse_objects = fo$sse, r2_objects = r_squared(fo$pred, human_objects))
}) |> purrr::list_rbind()
print(ablation)
cat("\n")

## ---- 3. Model comparison: what's uncfam competing against? ----------------
## Directly answers Review 1: "What's the model competing against? Are
## there models that wouldn't show this pattern?"
cat("=== 3. Model comparison ===\n")
fast_stoch <- xslControl(n_sim = 50) # reduced from the 500-sim default for tractable DEoptim runtimes

model_specs <- list(
  uncfam_entropy = list(model = uncfam(X = .1, B = .5, C = 1, variant = "entropy"),
                       lower = c(0, 0, .8, 0), upper = c(1, 10, 1, 20)),
  rescorla_wagner = list(model = rescorla_wagner(C = 1, alpha = .1, beta = .1, lambda = 1),
                        lower = c(.8, 0, 0, 0, 0), upper = c(1, 2, 2, 5, 20)),
  guess_and_test = list(model = guess_and_test(f = .1, sa = .5),
                       lower = c(0, 0, 0), upper = c(1, 1, 20)),
  pursuit = list(model = pursuit(gamma = .2, threshold = .3, lambda = .05),
                lower = c(0, 0, 0, 0), upper = c(1, 1, 1, 20)),
  propose_but_verify = list(model = propose_but_verify(alpha = .5, alpha_increase = .1),
                           lower = c(0, 0, 0), upper = c(1, 1, 20)),
  baseline = list(model = baseline(), lower = c(0), upper = c(20)) # phi only, no model params
)

comparison <- map(names(model_specs), function(nm) {
  spec <- model_specs[[nm]]
  fw <- fit_model(spec$model, words_dat, score_words_as_cues, human_words,
                  lower = spec$lower, upper = spec$upper, control = fast_stoch,
                  deoptim_control = DEoptim::DEoptim.control(NP = 15, itermax = 15, trace = FALSE))
  fo <- fit_model(spec$model, objects_dat, score_objects_as_cues, human_objects,
                  lower = spec$lower, upper = spec$upper, control = fast_stoch,
                  deoptim_control = DEoptim::DEoptim.control(NP = 15, itermax = 15, trace = FALSE))
  tibble::tibble(model = nm,
                sse_words = fw$sse, r2_words = r_squared(fw$pred, human_words),
                sse_objects = fo$sse, r2_objects = r_squared(fo$pred, human_objects))
}) |> purrr::list_rbind()

print(comparison)
