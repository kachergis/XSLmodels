# Define a simple likelihood function that only updates appropriate rows and columns
likelihoodFun = function(words, objects, alpha, delta) {
  ## With alpha=0, this enforces a mutual exclusivity constraint...
  mat = outer(words,objects) + outer(!words,!objects)
  ## Can relax mutual exclusivity contraint by increasing alpha or allowing for one of the following
  ## 1) Objects can have multiple words applied to them
  # mat = outer(words,objects) + outer(!words,ones)
  ## 2) Words can refer to multiple objects
  # mat = outer(words,objects) + outer(ones,!objects)
  likelihood = alpha^(1-mat) + (delta-1) * outer(words,objects)
  return(likelihood)
}

bayesian_decay_model <- function(params, data, reps) {
  # Define noise probability (when alpha = 0, Bayesian model is a deterministic 'ideal observer')
  alpha <- params[["alpha"]] # 0.1, 0.5, 0.9 decay for word/object non-co-occurrence
  delta <- params[["delta"]] # Multiplier for word/object co-occurrence (set to 1 for no increase)
  chDec <- params[["chDec"]] # Decision parameter (e.g., 1)
  voc = unique(unlist(data$words))
  ref = unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz = length(voc) # vocabulary size
  ref_sz = length(ref) # number of objects

  perf = matrix(0, reps, voc_sz) # a row for each block
  traj = list()

  ones = rep(1, voc_sz)
  pWgO <- matrix(1/ref_sz, voc_sz, ref_sz) # prob(word|object) matrix
  pW_O <- matrix(1/(ref_sz*voc_sz), voc_sz, ref_sz)
  colnames(pWgO) = ref
  rownames(pWgO) = voc
  colnames(pW_O) = ref
  rownames(pW_O) = voc
  # training
  for(rep in 1:reps) { # for trajectory experiments, train multiple times
    for(t in 1:length(data$words)) {
      tr_w = unlist(data$words[t])
      tr_w = tr_w[!is.na(tr_w)]
      tr_w = tr_w[tr_w != ""]
      tr_o = unlist(data$objects[t])
      tr_o = tr_o[!is.na(tr_o)]

      words_tr = rep(0,voc_sz)
      objects_tr = rep(0,ref_sz)
      names(words_tr) = voc
      names(objects_tr) = ref
      words_tr[tr_w] = 1
      objects_tr[tr_o] = 1

      likelihood = likelihoodFun(words_tr, objects_tr, alpha, delta)
      pWgO = likelihood * pWgO
      pWgO = pWgO/outer(ones,colSums(pWgO)) # rowSums(pWgO)
      pW_O = likelihood * pW_O
      pW_O = pW_O/sum(pW_O)

      # if(verbose) {
      #   cat('Conditional Probability of Words (rows) given Objects (columns)\n')
      #   print(pWgO)
      #   cat('Joint Probability of Words (rows) and Objects (columns)\n')
      #   print(pW_O)
      # }

      index = (rep-1)*length(data$words) + t # index for learning trajectory
      traj[[index]] = pWgO
    }
    # power choice rule
    perf[rep,] = get_perf(pWgO) # same as diag(pWgO)^chDec / colSums(pWgO^chDec)
  }
  #perf = diag(pWgO) / rowSums(pWgO) # no, we'll use parameterized choice
  # calculated from joint probs
  #perf = diag(pW_O)^chDec / sum(pW_O^chDec) # rowSums(pW_O^chDec)

  #cat('\nChoice Probabilities for each word given each object (using exp choice rule):\n')
  #chProb = exp(chDec*pWgO) / outer(ones,colSums(exp(chDec*pWgO)))
  #cat(diag(chProb))

  list(perf=perf, matrix=pWgO, traj=traj)
}

#' @export
bayesian_decay <- function(alpha, delta, chDec) {
  xslMod(
    name = "bayesian_decay",
    description = "Bayesian model of cross situational learning (originally conceived by Stephen Denton, Apr. 20, 2010)",
    model = bayesian_decay_model,
    params = c(alpha = alpha, delta = delta, chDec = chDec)
  )
}
