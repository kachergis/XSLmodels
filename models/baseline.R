# simple cooccurrence-counting baseline model

modelInfo <- list(label = "baseline co-occurrence counting",
                  model = function(params=c(), ord, reps=1) {
                    voc = unique(unlist(ord$words))
                    ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
                    voc_sz = length(voc) # vocabulary size
                    ref_sz = length(ref) # number of objects
                    traj = list()
                    m <- matrix(0, voc_sz, ref_sz) # association matrix
                    colnames(m) = ref
                    rownames(m) = voc
                    perf = matrix(0, reps, voc_sz) # a row for each block
                    # training
                    for(rep in 1:reps) { # for trajectory experiments, train multiple times
                      for(t in 1:length(ord$words)) {
                        tr_w = unlist(ord$words[t])
                        tr_w = tr_w[!is.na(tr_w)]
                        tr_w = tr_w[tr_w != ""]
                        tr_o = unlist(ord$objs[t])
                        tr_o = tr_o[!is.na(tr_o)]

                        m[tr_w,tr_o] = m[tr_w,tr_o] + 1

                        index = (rep-1)*length(ord$words) + t # index for learning trajectory
                        traj[[index]] = m
                      }
                      perf[rep,] = get_perf(m)
                    }
                    want = list(perf=perf, matrix=m, traj=traj)
                    return(want)
                  }
                  )


