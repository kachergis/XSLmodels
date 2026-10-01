# Model comparison on Experiment 1 of Gangwani, Kachergis & Yu, "Simultaneous
# Cross-situational Learning of Category and Object Names"
# (data-raw/add_gangwani2011_category.R).
#
# This is a hierarchical design (2 objects + 3 words/trial: two 1-to-1 names
# plus one 1-to-many category label), so its 15x12 association matrix does not
# fit xslData's diagonal scoring convention and xsl_run()'s built-in SSE can't
# be used. This script provides `score_gangwani2011_category()`, which reads
# the three quantities of interest straight off any model's returned matrix:
#
#   1. name accuracy      -- 12-AFC Luce choice for words 1-12
#   2. category accuracy  -- 3-AFC choice for the category labels (words 13-15),
#                            averaged over which category members are on screen
#   3. ME violation       -- P(learn the name AND the category label for the
#                            same object), the paper's headline result
#
# A softmax discrimination phi is fit as one extra free parameter per model
# (as in kachergis2012_highlighting_fit.R), uniformly across every candidate.

devtools::load_all("..")
suppressMessages(library(purrr))

blocks <- names(gangwani2011_category)
category_members <- list(`13` = 1:4, `14` = 5:8, `15` = 9:12)

softmax_rows <- function(m, phi) {
  e <- exp(phi * m)
  e / rowSums(e)
}

# --- the scorer -----------------------------------------------------------
# `m` is a 15 x 12 word-by-object matrix from xsl_run(mod, block)$fits[[1]]$matrix
score_gangwani2011_category <- function(m, phi = 1) {
  sm <- softmax_rows(m, phi)

  # 1. names: 12-AFC, correct object shares the word index
  name_acc <- diag(sm[1:12, 1:12])

  # 2 & 3. category labels: 3-AFC over one member per category
  other_cats <- list(`13` = list(5:8, 9:12),
                     `14` = list(1:4, 9:12),
                     `15` = list(1:4, 5:8))
  cat_acc <- setNames(numeric(3), 13:15)
  for (L in 13:15) {
    tgt <- category_members[[as.character(L)]]
    d1s <- other_cats[[as.character(L)]][[1]]
    d2s <- other_cats[[as.character(L)]][[2]]
    grid <- expand.grid(t = tgt, d1 = d1s, d2 = d2s)
    p <- sm[L, ]
    cat_acc[as.character(L)] <-
      mean(p[grid$t] / (p[grid$t] + p[grid$d1] + p[grid$d2]))
  }

  # ME violation: per object, P(name learned) * P(its category label learned),
  # averaged over objects (independence approximation -- the same approximation
  # applied to the human response_matrix below, for a like-for-like number).
  label_of <- rep(13:15, each = 4)
  me_violation <- mean(name_acc * cat_acc[as.character(label_of)])

  list(name_acc = name_acc,
       cat_acc = unname(cat_acc),
       me_violation = me_violation)
}

# human ME violation, from the (already row-normalised) response_matrix, under
# the same independence approximation
human_me_violation <- function(block) {
  rm <- block$response_matrix
  name_acc <- diag(rm[1:12, 1:12])
  label_of <- rep(13:15, each = 4)
  cat_acc <- vapply(13:15, function(L) sum(rm[L, category_members[[as.character(L)]]]), numeric(1))
  mean(name_acc * cat_acc[label_of - 12])
}

# --- fit one model (its own params + a trailing softmax phi) --------------
fit_one <- function(model, block, lower, upper,
                    control = xslControl(n_sim = 100),
                    deoptim_control = DEoptim::DEoptim.control(NP = 15, itermax = 15, trace = FALSE)) {
  npar <- sum(vapply(model$params, is.numeric, logical(1)))
  target <- block$accuracy                       # length 15: names 1-12, labels 13-15
  objective <- function(par) {
    val <- tryCatch({
      mod <- if (npar > 0) update_params(model, par[seq_len(npar)]) else model
      phi <- par[npar + 1]
      mat <- suppressWarnings(xsl_run(mod, block, control = control)$fits[[1]]$matrix)
      s <- score_gangwani2011_category(mat, phi)
      sum((c(s$name_acc, s$cat_acc) - target) ^ 2)
    }, error = function(e) NA_real_)
    if (is.na(val)) Inf else val
  }
  fit <- DEoptim::DEoptim(objective, lower = lower, upper = upper, control = deoptim_control)
  best <- fit$optim$bestmem
  mod <- if (npar > 0) update_params(model, best[seq_len(npar)]) else model
  mat <- suppressWarnings(xsl_run(mod, block, control = control)$fits[[1]]$matrix)
  list(par = best, sse = fit$optim$bestval,
       score = score_gangwani2011_category(mat, best[npar + 1]))
}

r2 <- function(pred, human) 1 - sum((pred - human)^2) / sum((human - mean(human))^2)

model_specs <- list(
  uncfam          = list(m = uncfam(X = .1, B = .5, C = 1), lo = c(0, 0, .8, 0), hi = c(1, 10, 1, 20)),
  rescorla_wagner = list(m = rescorla_wagner(C = 1, alpha = .1, beta = .1, lambda = 1),
                         lo = c(.8, 0, 0, 0, 0), hi = c(1, 2, 2, 5, 20)),
  pursuit         = list(m = pursuit(gamma = .2, threshold = .3, lambda = .05),
                         lo = c(0, 0, 0, 0), hi = c(1, 1, 1, 20)),
  baseline        = list(m = baseline(), lo = 0, hi = 20)   # phi only
)

cat("Human ME violation by block (independence approx off response_matrix;\n",
    "paper Fig. 7 measures it directly at ~.15-.30):\n", sep = "")
for (b in blocks) cat(sprintf("  %-18s %.3f\n", b, human_me_violation(gangwani2011_category[[b]])))
cat("Chance ME violation = 1/12 * 1/3 =", round(1/12/3, 3), "\n\n")

results <- map(blocks, function(bn) {
  block <- gangwani2011_category[[bn]]
  human <- block$accuracy
  me_h <- human_me_violation(block)   # precompute: tibble() data-masks `block` below
  map(names(model_specs), function(nm) {
    sp <- model_specs[[nm]]
    f <- fit_one(sp$m, block, sp$lo, sp$hi)
    pred <- c(f$score$name_acc, f$score$cat_acc)
    tibble::tibble(
      block = bn, model = nm, sse = f$sse,
      r2_all      = r2(pred, human),
      name_mean   = mean(f$score$name_acc),
      cat_mean    = mean(f$score$cat_acc),
      me_model    = f$score$me_violation,
      me_human    = me_h
    )
  }) |> list_rbind()
}) |> list_rbind()

print(as.data.frame(results), digits = 3)
