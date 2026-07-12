require(tidyverse)
require(XSLmodels)

# check against conditions in xsl_datasets:
conds <- c()
for(i in 1:length(xsl_datasets)) conds[i] = xsl_datasets[[i]]$condition
xsl_datasets[[6]] # "3x3 +1w/o"
xsl_datasets[[7]] # "4x4 +2w/o"

# generate info needed for xslData
summarize_condition <- function(dat) {
  response_matrix <- get_response_matrix(dat)
  item_acc <- dat |> group_by(CorrectAns) |>
    summarise(n = n(), # subjects
              acc = mean(Correct))
  sub_acc <- dat |> group_by(Subject) |>
    summarise(n = n(), # test items
              acc = mean(Correct))
  list(n_subj = length(unique(dat$Subject)),
       subj_perf_sd = sd(sub_acc$acc), # TODO: add to xslData
       test = item_acc$CorrectAns,
       accuracy = item_acc$acc,
       response_matrix = response_matrix)
}

get_asymmetric_trial_order <- function(fname) {
  tmp <- read.csv(fname, header = F)
  words <- tmp[seq(from=1, to=nrow(tmp), 2),] # odd rows are words
  objects <- tmp[seq(from=2, to=nrow(tmp), 2),] # even rows are objects
  words <- lapply(words, function(x) { unlist(strsplit(x, " ")) })
  objects <- lapply(objects, function(x) { unlist(strsplit(x, " ")) })
  list(words = words, objects = objects)
}

get_symmetric_trial_order <- function(fname, repetitions=1) {
  tmp <- read.csv(fname, header = F, sep='\t')
  words = list()
  index = 1
  for(rep in 1:repetitions) {
    for(i in 1:nrow(tmp)) {
      words[[index]] = unlist(tmp[i,])
      index = index + 1
    }
  }
  list(words = words, objects = words)
}

get_response_matrix <- function(dat) {
  voc_sz = max(dat$CorrectAns) # FIXME: base this on training trials!
  m <- matrix(0, nrow = voc_sz, ncol = voc_sz)
  for(i in 1:nrow(dat)) {
    m[dat[i,]$CorrectAns, dat[i,]$Response] = m[dat[i,]$CorrectAns, dat[i,]$Response] + 1
  }
  return(m)
}

# priming data (frequency..)
load("data-raw/data/priming_all_trajectory.Rdata") # each person does 4 consecutive blocks of the same training (and test)
prime_conds = unique(all$Exp) #
# "4 Pairs/Trial, 6x" - orig_4x4
# "3 Pairs/Trial 3,6,9x" - block2_3_6_9-3x3.txt
# "4 Pairs/Trial 3,6,9x" = x8_369.txt
sort(unique(all$TotalFreq)) # 3  6  9 12 18 24 27 36
prime <- all |>
  rename(Condition = Exp)
# TODO: figure out how to store response_matrix trajectories (block x word x object?) and accuracy (block x word?)
# for now, should just create a condition for each duration of training (1-4x copies of training trials)
ord_files = paste0(c("orig_4x4", "block2_3_6_9-3x3", "x8_369"), ".txt")

orig1x = get_symmetric_trial_order(paste0("data-raw/orders/", ord_files[1]), repetitions=1)
orig2x = get_symmetric_trial_order(paste0("data-raw/orders/", ord_files[1]), repetitions=2)
orig3x = get_symmetric_trial_order(paste0("data-raw/orders/", ord_files[1]), repetitions=3)
orig4x = get_symmetric_trial_order(paste0("data-raw/orders/", ord_files[1]), repetitions=4)

# for each condition, for block 1-4 ...
dat <- summarize_condition(prime |> filter(Condition==prime_conds[1], Block==1))
xslData(
  train = orig1x,
  #test = x4.1$test, # default to all objects - needs to be a list?
  test = list(),
  accuracy = dat$accuracy,
  n_subj = dat$n_subj,
  #subj_perf_sd = x4.1$subj_perf_sd,
  label = "4x4 1x",
  condition = "4x4 1 rep",
  description = "Original 4x4, 1 repetition.",
  response_matrix = dat$response_matrix
)

dat <- summarize_condition(prime |> filter(Condition==prime_conds[2], Block==1))
xslData(
  train = orig2x,
  #test = x4.1$test, # default to all objects - needs to be a list?
  test = list(),
  accuracy = dat$accuracy,
  n_subj = dat$n_subj,
  #subj_perf_sd = x4.1$subj_perf_sd,
  label = "4x4 2x",
  condition = "4x4 2 reps",
  description = "Original 4x4, 2 repetitions.",
  response_matrix = dat$response_matrix
)

# x4 - 3x4 probabilistic subsets
# TODO: extract item groups based on x4_analysis_probabilistic_subset.r
x4 <- read.csv("data-raw/agg_data/x4-prob_6-26.txt", sep='\t') |>
  mutate(Subject = paste0("x4_",Subject))
conds = unique(x4$Condition)
# "1_80per" - 3 words, 4 objects - each of the 18 objects appears 2 extra times (without it's word)
# "2_12-100per_6-50per" - 3x4 with one additional item selected from 6, each repeated 6 times; 12 100%, 6 50%
#  - 50% items are indices 13-18
# "3_6-100per_12-66per" - 3x4 with one additional item selected from 12; (6 100%; 12 66%)
#  - 12 66% items: 3, 4, 5, 6, 7, 8, 13, 14, 15 16, 17, 18
# "4_6-dist"
ord_files = c("80per.txt", "12-100per_6-50per.txt", "6-100per_12-66per.txt", "6-dist.txt")

x6 <- read.csv("data-raw/agg_data/x6-tc_data9-9.txt", sep='\t') |>
  rename(Condition = ExperimentName,
         RT = TestSlide.RT,
         TestIndex = TestList.Sample) |> select(-SessionTime) |>
  mutate(Subject = paste0("x6_",Subject))

x4 <- x4 |> bind_rows(x6 |>
                        filter(Condition %in% c("1_80per", "2_12-100per_6-50per", "3_6-100per_12-66per")))


x4.1 <- summarize_condition(subset(x4, Condition==conds[1]))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/80per.txt"),
  #test = x4.1$test, # default to all objects - needs to be a list?
  test = list(),
  accuracy = x4.1$accuracy,
  n_subj = x4.1$n_subj,
  #subj_perf_sd = x4.1$subj_perf_sd,
  label = "201",
  condition = "3x4 6/8",
  description = "3x4, with one additional object per trial selected from all 18. A probabilistic condition with P(w|o) = .75 (6/8) for every pair.",
  response_matrix = x4.1$response_matrix
)

x4.2 <- summarize_condition(subset(x4, Condition==conds[2]))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/12-100per_6-50per.txt"),
  #test = x4.2$test, # default to all objects
  test = list(),
  accuracy = x4.2$accuracy,
  response_matrix = x4.2$response_matrix,
  n_subj = x4.2$n_subj,
  #subj_perf_sd = x4.2$subj_perf_sd,
  label = "3x4 1/.5",
  condition = "12-100per_6-50per", # 3x4 ...
  description = "3x4 with one additional object selected from 6 objects, each repeated 6 times. Thus, 12 word-object pairs have P(w|o) = 1, while 6 pairs have P(w|o) = .5"
)

x4.3 <- summarize_condition(subset(x4, Condition==conds[3]))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/6-100per_12-66per.txt"),
  test = x4.3$test, # default to all objects
  accuracy = x4.3$accuracy,
  response_matrix = x4.3$response_matrix,
  n_subj = x4.3$n_subj,
  #subj_perf_sd = x4.3$subj_perf_sd,
  label = "3x4 1/.66",
  condition = "6-100per_12-66per", # 3x4 ...
  description = "3x4 with one additional object selected from 12. 6 pairs have P(w|o) = 1, and 12 pairs have P(w|o) = .66."
)

x4.4 <- summarize_condition(subset(x4, Condition==conds[4]))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/6-dist.txt"),
  test = x4.4$test, # default to all objects
  accuracy = x4.4$accuracy,
  response_matrix = x4.4$response_matrix,
  n_subj = x4.4$n_subj,
  #subj_perf_sd = x4.4$subj_perf_sd,
  label = "3x4 +6o",
  condition = "3x4 +6", #
  description = "3x4 with one addition item selected from a set of 6 novel objects (each repeated 6 times)"
)


conds = unique(x6$Condition)
# conditions - reordering of original 4x4
# Cond 1_ : Mean Temp Cont per Trial: 1.0
# Cond 2_ : Mean Temp Cont per Trial: 1.40740740741
# Cond 3_ : Mean Temp Cont per Trial: 0.666666666667
# Cond 4_ : Mean Temp Cont per Trial: 0.333333333333

# Cond 1_ : Original 4x4; Mean Temp Cont per Trial: 1.0
x6.1 <- summarize_condition(subset(x6, Condition=="1_"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/reord_orig_1.txt"),
  test = x6.1$test, # default to all objects - needs to be a list?
  accuracy = x6.1$accuracy,
  response_matrix = x6.1$response_matrix,
  n_subj = x6.1$n_subj,
  #subj_perf_sd = x6.1$subj_perf_sd,
  label = character("4x4 1 overlap"),
  condition = character("4x4 shuffle 1 pair overlap"),
  description = "shuffled original 4x4 with mean trial-to-trial overlap of 1 pair"
)

x6.2 <- summarize_condition(subset(x6, Condition=="2_"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/reord_orig_2.txt"),
  test = x6.2$test, # default to all objects - needs to be a list?
  accuracy = x6.2$accuracy,
  response_matrix = x6.2$response_matrix,
  n_subj = x6.2$n_subj,
  #subj_perf_sd = x6.2$subj_perf_sd,
  label = character("4x4 1.4 overlaps"),
  condition = character("4x4 shuffle 1.41 pairs overlap"),
  description = "shuffled original 4x4 with mean trial-to-trial overlap of 1.41 pairs"
)

x6.3 <- summarize_condition(subset(x6, Condition=="3_"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/reord_orig_3.txt"),
  test = x6.3$test,
  accuracy = x6.3$accuracy,
  response_matrix = x6.3$response_matrix,
  n_subj = x6.3$n_subj,
  #subj_perf_sd = x6.3$subj_perf_sd,
  label = character("4x4 0.67 overlaps"),
  condition = character("4x4 shuffle 0.67 pairs overlap"),
  description = "shuffled original 4x4 with mean trial-to-trial overlap of 0.67 pairs"
)

x6.4 <- summarize_condition(subset(x6, Condition=="3_"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/reord_orig_4.txt"),
  test = x6.4$test,
  accuracy = x6.4$accuracy,
  response_matrix = x6.4$response_matrix,
  n_subj = x6.4$n_subj,
  #subj_perf_sd = x6.2$subj_perf_sd,
  label = character("4x4 0.33 overlaps"),
  condition = character("4x4 shuffle 0.33 pairs overlap"),
  description = "shuffled original 4x4 with mean trial-to-trial overlap of 0.33 pairs"
)

# x7-parameter - see x7_analysis.r
#x7 <- read.csv("data-raw/agg_data/x7-data-10-19corrected.txt", sep='\t') # 2844 rows
x7 <- read.csv("data-raw/agg_data/x7-data-10-19.txt", sep='\t') |> # 2340
  rename(Condition = ExperimentName,
         RT = TestSlide.RT,
         TestIndex = TestList.Sample) |> select(-SessionTime) |>
  mutate(Experiment = "x7-parameter",
         Subject = paste0("x7_",Subject))
conds = unique(x7$Condition) # "1_1x4"  "2_1x3"  "3_3x3_1nsy"  "4_4x4_2nsy"
# 1x4, 1x3, 3x3 with 1 noisy, 4x4 with 2 noisy

x7.1 <- summarize_condition(subset(x7, Condition=="1_1x4"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/1x4.txt"),
  test = x7.1$test,
  accuracy = x7.1$accuracy,
  response_matrix = x7.1$response_matrix,
  n_subj = x7.1$n_subj,
  #subj_perf_sd = x7.1$subj_perf_sd,
  label = character("1x4"),
  condition = character("1x4"),
  description = "based on original 4x4, but with only 1 word and 4 referents per trial"
)

x7.2 <- summarize_condition(subset(x7, Condition=="2_1x3"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/1x3.txt"),
  test = x7.2$test,
  accuracy = x7.2$accuracy,
  response_matrix = x7.2$response_matrix,
  n_subj = x7.2$n_subj,
  #subj_perf_sd = x7.2$subj_perf_sd,
  label = character("1x3"),
  condition = character("1x3"),
  description = "based on original 3x3, but with only 1 word and 3 referents per trial"
)

x7.3 <- summarize_condition(subset(x7, Condition=="3_3x3_1nsy"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/3x3_1nsy.txt"),
  test = x7.3$test,
  accuracy = x7.3$accuracy,
  response_matrix = x7.3$response_matrix,
  n_subj = x7.3$n_subj,
  #subj_perf_sd = x7.3$subj_perf_sd,
  label = character("3x3 1nsy"),
  condition = character("3x3 1 noisy"),
  description = "based on original 3x3, but with only 1 word and 4 referents per trial"
)


x7.4 <- summarize_condition(subset(x7, Condition=="4_4x4_2nsy"))
xslData(
  train = get_asymmetric_trial_order("data-raw/orders/4x4_2nsy.txt"),
  test = x7.3$test,
  accuracy = x7.3$accuracy,
  response_matrix = x7.3$response_matrix,
  n_subj = x7.3$n_subj,
  #subj_perf_sd = x7.3$subj_perf_sd,
  label = character("4x4 2nsy"),
  condition = character("4x4 2 noisy"),
  description = "based on original 4x4, but showing only 2 correct pairs per trial along with 2 noisy words and objects"
)


# x8-diff-freq
x8 <- read.csv("data-raw/agg_data/x8_9-22-08.txt", sep='\t') |>
  rename(Condition = ExperimentName,
         RT = TestSlide.RT,
         TestIndex = TestList.Sample) |> select(-SessionTime) |>
  mutate(Experiment = "x8-diff-freq",
         Subject = paste0("x8_",Subject))
conds = unique(x8$Condition)
# "1_" = max temp cont and spat cont 1_max_temp_spat_cont_orig.txt (reorder of original 4x4)
# "2_" = (3reps (10-18),9reps (1-9)) 2_x8_39.txt
# "3_" = (3reps (1-6),6reps (7-12),9reps (13-18)) 3_x8_369.txt
# "4_" = max temp cont 1_max_temp_spat_cont_orig.txt (but not spatial contiguity)
# 33 participants, 18 responses

# raw <- subset(raw, Condition=="1_" | Condition=="4_")
raw <- subset(raw, Condition=="4_")
attach(raw)
# max temp cont degree of overlap analysis
# a0rep <- subset(raw, (CorrectAns==2 | CorrectAns==5 | CorrectAns==7 | CorrectAns==11))
# a1rep <- subset(raw, CorrectAns==11)
# a2rep <- subset(raw, CorrectAns==4 | CorrectAns==5)
# a3rep <- subset(raw, CorrectAns==3 | CorrectAns==7 | CorrectAns==8 | CorrectAns==9 | CorrectAns==10 | CorrectAns==12 | CorrectAns>13)
# a4rep <- subset(raw, CorrectAns==1 | CorrectAns==6 | CorrectAns==13)
# a5rep <- subset(raw, CorrectAns==2)

get_response_matrix(x8 |> filter(Condition=="1_"))
get_response_matrix(x8 |> filter(Condition=="2_"))
get_response_matrix(x8 |> filter(Condition=="3_"))

# x9-cont_div
x9 <- read.csv("data-raw/agg_data/x9_10-19.txt", sep='\t') |>
  rename(Condition = ExperimentName)
x9.2 <- read.csv("data-raw/agg_data/x9_block1_10-19.txt", sep='\t') |>
  rename(Condition = ExperimentName) # just the recognition memory test block
x9 <- x9 |>
  mutate(Experiment = "x9-cont_div",
         Subject = paste0("x9_",Subject))
conds = unique(x9$ExperimentName) # "2_" "3_" "4_"
x9_rmat <- list()
x9_perf <- list()
for(cond in conds) {
  x9_rmat[[cond]] <- get_response_matrix(x9 |> filter(ExperimentName == cond))
  x9_perf[[cond]] <- diag(x9_rmat[[cond]]) / rowSums(x9_rmat[[cond]])
}
# 1_	recognition memory testing 9 words 9 objects
# 2_	???
# 3_	cont_div
# 4_	orig_aut_order


# x10-cont_div2
x10 <- read.csv("data-raw/agg_data/x10-11-12.txt", sep='\t') |>
  rename(Condition = ExperimentName) |>
  mutate(Subject = paste0("x10_",Subject),
         order = ifelse(Condition=="2_", "3_6_9-3x3",
                        ifelse(Condition=="3_", "cont_div6-12", # 3x3
                               ifelse(Condition=="4_", "3x3_orig_order", NA))))
conds = unique(x10$order)



# x12-cont_div3
x12 <- read.csv("data-raw/agg_data/x12-11-12.txt", sep='\t') |>
  rename(Condition = ExperimentName) |>
  mutate(Subject = paste0("x12_",Subject),
         order = ifelse(Condition=="1_", "block1_3_6_9-3x3-lo_cd",
                        ifelse(Condition=="2_", "block2_3_6_9-3x3",
                               ifelse(Condition=="3_", "block3_3_6_9_lo_medCD",
                                      ifelse(Condition=="4_", "block4_369_39mx", NA)))))
conds = unique(x12$order)

x13 <- read.csv("x13_1-21.txt", sep='\t')
# Conds: 1_ is 1olap2tr_contr
#			   2_ is 1olap3tr_contr
#			   3_ is 3_2olap2tr_contr
#			   4_ is 3_6_9 medCD (3 & 9 freq co-occur) 3x3 369_39mx
# for conditions 1,2,&3:
# 1-6 get repeated with each other (2 it olap)
#  7-12 can appear with 1-6 when they are repeated
#  13-18 only appear on discontiguous trials



# filtering
load("data-raw/data/filtering_data.RData")

acc_s <- raw |>
  rename(Condition = Cond) |>
  mutate(order = paste0("filt",EarlyRepetitions,"E_", Late,"L")) |>
  group_by(order, Word, AtoA, EarlyRepetitions, Late) |> # early,
  summarise(Correct = mean(Correct), n=n())

acc_s |> group_by(AtoA, EarlyRepetitions, Late) |>
  summarise(correct = mean(Correct)) |>
  ggplot(aes(x=EarlyRepetitions, y=correct, group=AtoA, color=AtoA)) + facet_wrap(vars(Late)) + geom_line() + theme_bw()

conds = unique(raw$Condition)
ord_files = unique(acc_s$order) # paste0(order, ".txt")



# looks like this is the implicit learning experiment, asking for the number of cooccurrences of particular word-object combinations
x5 <- read.csv("data-raw/agg_data/x5-9-19.txt", sep='\t') |>
  rename(Condition = ExperimentName,
         RT = testWait.RT,
         Response = testWait.RESP,
         TestIndex = TestList.Sample,
         object = Picture,
         word = sound) |> select(-SessionTime) |>
  mutate(Subject = paste0("x5_",Subject))
conds = unique(x5$Condition) # "5_1" "5_2" "5_3" "5_4"
head(subset(x5, Condition=="5_2"))
head(subset(x5, Condition=="5_3"))
head(subset(x5, Condition=="5_4"))

x5 |> ggplot(aes(x=Cooccurrences, y=Response)) +
  geom_jitter(alpha=.3) + facet_wrap(. ~ Condition) +
  theme_classic()
