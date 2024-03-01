decay_model <- function(params, data, reps=1) {
  C = params[["C"]] # decay
  voc = unique(unlist(data$words))
  ref = unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz = length(voc) # vocabulary size
  ref_sz = length(ref) # number of objects
  traj = list()
  m <- matrix(0, voc_sz, ref_sz) # association matrix
  colnames(m) = ref
  rownames(m) = voc
  perf = matrix(0, reps, voc_sz) # a row for each block
  # training
  for(rep in 1:reps) { # for trajectory experiments, train multiple times
    for(t in 1:length(data$words)) {
      tr_w = unlist(data$words[t])
      tr_w = tr_w[!is.na(tr_w)]
      tr_w = tr_w[tr_w != ""]
      tr_o = unlist(data$objects[t])
      tr_o = tr_o[!is.na(tr_o)]

      m = m*C # bestvalit: 0.618814 bestmemit:    0.990905
      m[tr_w,tr_o] = m[tr_w,tr_o] + 1

      index = (rep-1)*length(data$words) + t # index for learning trajectory
      traj[[index]] = m
    }
    perf[rep,] = get_perf(m)
  }
  want = list(perf=perf, matrix=m, traj=traj)
  return(want)
}

decay <- function(C) {
  xslMod(
    name = "decay",
    description = "Simple cooccurrence-counting baseline model",
    model = decay_model,
    params = c(C = C)
  )
}
