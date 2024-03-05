ex <- xsl_datasets[[10]]

xsl_run(baseline(), ex)
xsl_run(decay(C = .98), ex)
xsl_run(decay(C = .98), ex, reps = 3)
xsl_run(kachergis(X = .1, C = 1, B = .98), data = ex)
xsl_run(kachergis(X = .1, C = 1, B = .98), data = ex, reps = 3)
xsl_run(bayesian_decay(alpha = 0.5, delta = 1, chDec = 1), data = ex)

xsl_run(baseline(), xsl_datasets[1:3])
xsl_run(kachergis(X = .1, C = 1, B = .98), data = xsl_datasets[1:3])
xsl_run(guess_and_test(f = .7, sa = 0.9), data = xsl_datasets[1:3])

xsl_fit(decay(C = .98), data = ex, lower = .8, upper = 1)
xsl_fit(decay(C = .98), data = xsl_datasets[1:3], lower = .8, upper = 1, reps = 3)

xsl_fit(guess_and_test(f = .7, sa = 0.9), data = ex, lower = c(0.1, 0.1), upper = c(0.1, 0.99))

xsl_run(kachergis(X = .1, C = 1, B = .98), data = xsl_datasets[1:3])
xsl_run(kachergis(X = .1, C = 1, B = .98), data = xsl_datasets[1:3], reps = 3)
xsl_run_s(kachergis(X = .1, C = 1, B = .98), data = xsl_datasets[1:3])
