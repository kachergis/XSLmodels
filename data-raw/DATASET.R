## code to prepare datasets goes here

# ToDo: update and move process_FMcorpus and process_FGTcorpus functions
# from load_corpus_data.R

# process_FGTcorpus ...
#usethis::use_data(FGT_corpus, overwrite = TRUE)

# process_FMcorpus ...
#usethis::use_data(FGT_corpus, overwrite = TRUE)


modelFiles <- list.files(path="models/", pattern="\\.R$", recursive=T)

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R$", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(paste0("models/",modelFiles[i]))
  modelInfo <- lapply(modelInfo, rlang::zap_srcref)
  models[[i]] <- modelInfo
  rm(modelInfo)
}


usethis::use_data(models, overwrite = TRUE)
#save(models, file="data/models.rds", version=2)


# ToDo: find the combined_data creating code and put it here
