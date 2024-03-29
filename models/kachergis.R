# Associative Uncertainty- (Entropy) & Familiarity-Biased Model
# George Kachergis  george.kachergis@gmail.com

# for testing:
#mat = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
# ord = list(words=mat, objs=mat)
#mat2 = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
# ord = list(words=mat2, objs=mat2)


modelInfo <- list(
  label = "Kachergis et al. 2012 uncertainty- and familiarity-biased associative model",
  model = function(params, ord=c(), start_matrix=c(), reps=1, test_noise=0) {
    X <- params[1] # associative weight to distribute
    B <- params[2] # weighting of uncertainty vs. familiarity
    C <- params[3] # decay

    voc = unique(unlist(ord$words))
    ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
    voc_sz = length(voc) # vocabulary size
    ref_sz = length(ref) # number of objects
    traj = list()
    if(is.matrix(start_matrix)) {
      m <- start_matrix
    } else {
      m <- matrix(0, voc_sz, ref_sz) # association matrix
    }
    colnames(m) = ref
    rownames(m) = voc
    perf = matrix(0, reps, voc_sz) # a row for each block
    # training
    for(rep in 1:reps) { # for trajectory experiments, train multiple times
      for(t in 1:length(ord$words)) {
        #print(format(m, digits=3))

        tr_w = unlist(ord$words[t])
        tr_w = tr_w[!is.na(tr_w)]
        tr_w = tr_w[tr_w != ""]
        tr_o = unlist(ord$objs[t])
        tr_o = tr_o[!is.na(tr_o)]

        m = update_known(m, tr_w, tr_o) # what's been seen so far?
        if(length(tr_w)>1) {
          ent_w = apply(m[tr_w,], 1, shannon.entropy)
        } else {
          ent_w = shannon.entropy(m[tr_w,])
        }
        ent_w = exp(B*ent_w)

        ent_o = apply(as.matrix(m[,tr_o]), 2, shannon.entropy)
        ent_o = exp(B*ent_o)

        nent = (ent_w %*% t(ent_o))
        #nent = nent / sum(nent)
        # get all current w,o strengths and normalize to distr X
        assocs = m[tr_w,tr_o]
        denom = sum(assocs * nent)
        m = m*C # decay everything
        # update associations on this trial
        m[tr_w,tr_o] = m[tr_w,tr_o] + (X * assocs * (ent_w %*% t(ent_o))) / denom

        index = (rep-1)*length(ord$words) + t  # index for learning trajectory
        traj[[index]] = m
      }
      m_test = m+test_noise # test noise constant k
      perf[rep,] = get_perf(m_test)
    }
    want = list(perf=perf, matrix=m, traj=traj)
    return(want)
  }
)

