require(tidyverse)
require(XSLmodels)

get_response_matrix <- function(dat) {
  voc_sz = max(dat$CorrectAns) # FIXME: base this on training trials!
  m <- matrix(0, nrow = voc_sz, ncol = voc_sz)
  for(i in 1:nrow(dat)) {
    m[dat[i,]$CorrectAns, dat[i,]$Response] = m[dat[i,]$CorrectAns, dat[i,]$Response] + 1
  }
  return(m)
}

# priming data (frequency..)
load("priming_all_trajectory.Rdata") # each person does 4 consecutive blocks of the same training (and test)
conds = unique(all$Exp) #
# "4 Pairs/Trial, 6x" - orig_4x4
# "3 Pairs/Trial 3,6,9x" - block2_3_6_9-3x3.txt
# "4 Pairs/Trial 3,6,9x" = x8_369.txt
sort(unique(all$TotalFreq)) # 3  6  9 12 18 24 27 36
primedat <- all |> group_by(Exp, TotalFreq, Subject) |>
  summarise(perf = mean(Correct),
            n = n())
# TODO: figure out how to store response_matrix trajectories (block x word x object?) and accuracy (block x word?)


# x4 - 3x4 probabilistic subsets
# TODO: extract item groups based on x4_analysis_probabilistic_subset.r
x4 <- read.csv("x4-prob_6-26.txt", sep='\t') |>
  mutate(Subject = paste0("x4_",Subject))
conds = unique(x4$Condition)
# "1_80per"  "2_12-100per_6-50per"  "3_6-100per_12-66per"  "4_6-dist"
ord_files = c("80per.txt", "12-100per_6-50per.txt", "6-100per_12-66per.txt", "6-dist.txt")

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
       subj_perf_sd = sd(sub_acc$acc),
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

x4.1 <- summarize_condition(subset(x4, Condition==conds[1]))
xslData(
  train = get_asymmetric_trial_order("orders/80per.txt"),
  test = x4.1$test, # default to all objects - needs to be a list?
  accuracy = x4.1$accuracy,
  #response_matrix = x4.1$response_matrix,
  n_subj = x4.1$n_subj,
  #subj_perf_sd = x4.1$subj_perf_sd,
  label = character(),  # TODO
  condition = character(),
  description = "3x4, with one additional object per trial selected from all 18. A probabilistic condition with P(w|o) = .8 (6/8) for every pair."
)

x4.2 <- summarize_condition(subset(x4, Condition==conds[2]))
xslData(
  train = get_asymmetric_trial_order("orders/12-100per_6-50per.txt"),
  test = x4.2$test, # default to all objects
  accuracy = x4.2$accuracy,
  #response_matrix = x4.2$response_matrix,
  n_subj = x4.2$n_subj,
  #subj_perf_sd = x4.2$subj_perf_sd,
  label = character(),  # TODO
  condition = "12-100per_6-50per", # 3x4 ...
  description = "3x4 with one additional object selected from 6 objects, each repeated 6 times. Thus, 12 word-object pairs have P(w|o) = 1, while 6 pairs have P(w|o) = .5"
)

x4.3 <- summarize_condition(subset(x4, Condition==conds[3]))
xslData(
  train = get_asymmetric_trial_order("orders/6-100per_12-66per.txt"),
  test = x4.3$test, # default to all objects
  accuracy = x4.3$accuracy,
  #response_matrix = x4.3$response_matrix,
  n_subj = x4.3$n_subj,
  #subj_perf_sd = x4.3$subj_perf_sd,
  label = character(), # TODO
  condition = "6-100per_12-66per", # 3x4 ...
  description = "3x4 with one additional object selected from 12. 6 pairs have P(w|o) = 1, and 12 pairs have P(w|o) = .66."
)

x4.4 <- summarize_condition(subset(x4, Condition==conds[4]))
xslData(
  train = get_asymmetric_trial_order("orders/6-dist.txt"),
  test = x4.4$test, # default to all objects
  accuracy = x4.4$accuracy,
  #response_matrix = x4.4$response_matrix,
  n_subj = x4.4$n_subj,
  #subj_perf_sd = x4.4$subj_perf_sd,
  label = character(),  # TODO
  condition = "3x4 +6", #
  description = "3x4 with one addition item selected from a set of 6 novel objects (each repeated 6 times)"
)



x6 <- read.csv("agg_data/x6-tc_data9-9.txt", sep='\t') |>
  rename(Condition = ExperimentName,
         RT = TestSlide.RT,
         TestIndex = TestList.Sample) |> select(-SessionTime) |>
  mutate(Subject = paste0("x6_",Subject))
x6_80per <- subset(x6, Condition=="1_80per")
x6_100per_6 <- subset(x6, Condition=="2_12-100per_6-50per")
x6_100per_12 <- subset(x6, Condition=="3_6-100per_12-66per")
# TODO: pool participants from this experiment with conditions from experiment 4
conds = unique(x6$Condition)
# TODO: what are conditions 1_, 2_, 3_ and 4_ ??

x6.1 <- summarize_condition(subset(x6, Condition=="1_"))
xslData(
  train = get_asymmetric_trial_order(""), # TODO
  test = x6.1$test, # default to all objects - needs to be a list?
  accuracy = x6.1$accuracy,
  #response_matrix = x4.1$response_matrix,
  n_subj = x6.1$n_subj,
  #subj_perf_sd = x6.1$subj_perf_sd,
  label = character(),  # TODO
  condition = character(),
  #description = ""
)


# x7-parameter - see x7_analysis.r
x7 <- read.csv("x7-data-10-19corrected.txt", sep='\t') # 2844 rows
x7.2 <- read.csv("agg_data/x7-data-10-19.txt", sep='\t') # 2340
conds = unique(x7$Condition) # "1_1x4"  "2_1x3"  "3_3x3_1nsy"  "4_4x4_2nsy"
# 1x4, 1x3, 3x3 with 1 noisy, 4x4 with 2 noisy


# x8-diff-freq
x8 <- read.csv("agg_data/x8_9-22-08.txt", sep='\t') |>
  rename(Condition = ExperimentName,
         RT = TestSlide.RT,
         TestIndex = TestList.Sample) |> select(-SessionTime) |>
  mutate(Experiment = "x8-diff-freq",
         Subject = paste0("x8_",Subject))
conds = unique(x8$Condition) # "1_"  "2_"  "3_"  "4_"
# TODO: look up conditions


# x9-cont_div
x9 <- read.csv("agg_data/x9_10-19.txt", sep='\t') |>
  rename(Condition = ExperimentName)
x9.2 <- read.csv("agg_data/x9_block1_10-19.txt", sep='\t') |>
  rename(Condition = ExperimentName)
# TODO: bind_rows(x9, x9.2) ??
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
x10 <- read.csv("agg_data/x10-11-12.txt", sep='\t') |>
  rename(Condition = ExperimentName) |>
  mutate(Subject = paste0("x10_",Subject),
         order = ifelse(Condition=="2_", "3_6_9-3x3",
                        ifelse(Condition=="3_", "cont_div6-12", # 3x3
                               ifelse(Condition=="4_", "3x3_orig_order", NA))))
conds = unique(x10$order)


# x12-cont_div3
x12 <- read.csv("agg_data/x12-11-12.txt", sep='\t') |>
  rename(Condition = ExperimentName) |>
  mutate(Subject = paste0("x12_",Subject),
         order = ifelse(Condition=="1_", "block1_3_6_9-3x3-lo_cd",
                        ifelse(Condition=="2_", "block2_3_6_9-3x3",
                               ifelse(Condition=="3_", "block3_3_6_9_lo_medCD",
                                      ifelse(Condition=="4_", "block4_369_39mx", NA)))))
conds = unique(x12$order)


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
x5 <- read.csv("agg_data/x5-9-19.txt", sep='\t') |>
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
