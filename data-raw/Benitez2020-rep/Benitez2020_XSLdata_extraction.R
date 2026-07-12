# extract training orders, 2AFC test trials, and confusion matrices for Benitez et al. 2020
# The temporal structure of naming events differentially affects children’s and adults’ cross-situational word learning
require(tidyverse)

# data downloaded from https://osf.io/2hmxr/
all_data <- read.csv("CrossSitRep_allData.csv")

# expLabels: experiment name, corresponding to experiment number in manuscript (Exp1, Exp2, or Exp3)
# condition: experimental condition (unstructured, interleaved, massed)
# order: order parameter, indicating training trial list (1, 2, or 3)
# set: item set, indicating which set of object-label pairings was used (1 or 2)
unique_train <- subset(all_data, trial_type=="training") |>
  select(condition, order, set, trial_index, image1, image2, sound1, sound2) |> # group, expLabels,
  distinct()

subset(all_data, trial_type=="training") |>
  select(condition, trial_index, sound1, sound2) |> distinct() |> View() # image1, image2, 
# 56 trials - there are two Unstructured orders (although they have an idential trial 5)

train_dat <- subset(all_data, trial_type=="training") |>
  select(order, condition, trial_index, sound1, sound2) |> distinct() |> # image1, image2, 
  mutate(w1 = as.numeric(str_extract(sound1, "\\d+")),
         w2 = as.numeric(str_extract(sound2, "\\d+")))
# 69 trials

train_dat |> filter(condition=="Interleaved") |>
  select(w1, w2) |> 
  slice(1:12) |> # final 3 rows have same words as trials 2, 7, and 12, but played in different order
  write.table("Benitez2020_interleaved.txt", sep='\t', col.names=F, row.names=F)

train_dat |> filter(condition=="Massed") |>
  slice(1:12) |> # final 6rows have same words as earlier trials, but played in different order
  select(w1, w2) |> write.table("Benitez2020_massed.txt", sep='\t', col.names=F, row.names=F)

train_dat |> filter(condition=="Unstructured", order==1) |>
  select(w1, w2) |> write.table("Benitez2020_unstructured_order1.txt", sep='\t', col.names=F, row.names=F)

# GK verified that order 2 and 3 are the same
train_dat |> filter(condition=="Unstructured", order==3) |>
  select(w1, w2) |> write.table("Benitez2020_unstructured_order2n3.txt", sep='\t', col.names=F, row.names=F)


# test trials
tdat <- filter(all_data,trial_type=="test") |>
  mutate(o1 = as.numeric(str_extract(image1, "\\d+")),
         o2 = as.numeric(str_extract(image2, "\\d+")),
         w = as.numeric(str_extract(sound, "\\d+")),
         response = as.numeric(str_extract(selected_image, "\\d+"))) |>
  select(id, group, expLabels, condition, order, trial_index, w, o1, o2, response, accuracy)

# 2AFC test trials (words 1-8)
tdat |>
  filter(condition=="Interleaved") |>
  distinct(w, o1, o2) |> arrange(w) |> select(-w) |>
  write.table("Benitez2020_interleaved_test.txt", sep='\t', col.names=F, row.names=F)

tdat |>
  filter(condition=="Massed") |>
  distinct(w, o1, o2) |> arrange(w) |> select(-w) |>
  write.table("Benitez2020_massed_test.txt", sep='\t', col.names=F, row.names=F)

tdat |>
  filter(condition=="Unstructured", order==1) |>
  distinct(w, o1, o2) |> arrange(w) |> select(-w) |>
  write.table("Benitez2020_unstructured_order1_test.txt", sep='\t', col.names=F, row.names=F)


tdat |> # verified order 3 is same as 2
  filter(condition=="Unstructured", order==2) |>
  distinct(w, o1, o2) |> arrange(w) |> select(-w) |>
  write.table("Benitez2020_unstructured_order2n3_test.txt", sep='\t', col.names=F, row.names=F)


get_response_matrix <- function(cdat) {
  voc_sz = max(cdat$w)
  mat <- matrix(0, nrow=voc_sz, ncol=voc_sz)
  for(i in 1:nrow(cdat)) {
    mat[cdat[i,]$w, cdat[i,]$response] = mat[cdat[i,]$w, cdat[i,]$response] + 1
  }
  return(mat)
}

# response matrices, for kids and adults
get_response_matrix(tdat |> filter(condition=="Interleaved", group=="kids")) |>
  write.table("Benitez2020_interleaved_kids_responses.txt", sep='\t', col.names=F, row.names=F)
get_response_matrix(tdat |> filter(condition=="Interleaved", group=="adults")) |>
  write.table("Benitez2020_interleaved_adults_responses.txt", sep='\t', col.names=F, row.names=F)

get_response_matrix(tdat |> filter(condition=="Massed", group=="kids")) |>
  write.table("Benitez2020_massed_kids_responses.txt", sep='\t', col.names=F, row.names=F)
get_response_matrix(tdat |> filter(condition=="Massed", group=="adults")) |>
  write.table("Benitez2020_massed_adults_responses.txt", sep='\t', col.names=F, row.names=F)

get_response_matrix(tdat |> filter(condition=="Unstructured", order==1, group=="kids")) |>
  write.table("Benitez2020_unstructured_order1_kids_responses.txt", sep='\t', col.names=F, row.names=F)
#get_response_matrix(tdat |> filter(condition=="Unstructured", order==1, group=="adults")) |>
#  write.table("Benitez2020_unstructed_adults_responses.txt", sep='\t', col.names=F, row.names=F)


get_response_matrix(tdat |> filter(condition=="Unstructured", order!=1, group=="kids")) |>
  write.table("Benitez2020_unstructured_order2n3_kids_responses.txt", sep='\t', col.names=F, row.names=F)
get_response_matrix(tdat |> filter(condition=="Unstructured", order!=1, group=="adults")) |>
  write.table("Benitez2020_unstructured_order2n3_adults_responses.txt", sep='\t', col.names=F, row.names=F)

# ToDo: call XSL-data constructor for all of these conditions
