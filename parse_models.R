# not allowed to source files in an R package, so we need to
# load model functions in a list and save to an RData file
# (similar to caret package's approach)
# e.g.: https://github.com/topepo/caret/blob/master/models/files/Mlda.R

modelFiles <- list.files(path="models/", pattern="\\.R$", recursive=T)

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R$", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(paste0("models/",modelFiles[i]))
  modelInfo <- lapply(modelInfo, rlang::zap_srcref)
  models[[i]] <- modelInfo
  rm(modelInfo)
}


save(models, file="data/models.rda", version=2)
