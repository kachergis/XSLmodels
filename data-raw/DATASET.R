library(purrr)
load("combined_data.rda")

# read in spreadsheet with citation and naming info, merge with training order files and (processed?) data..

cds <- map2(combined_data, names(combined_data), function(cd, nm) {
  train <- list(words = map(cd$train$words, unname),
                objects = map(cd$train$objs, unname))
  if (is.null(cd$test)) {
    test <- NULL
  } else {
    test_trans <- transpose(cd$test$trials)
    test <- list(words = test_trans$word, objects = test_trans$objs)
  }
  xslData(train = train, test = test,
          accuracy = cd$HumanItemAcc, n_subj = cd$Nsubj,
          label = nm, condition = cd$Condition) # response_matrix = words x chosen object
  # citation
})

xsl_datasets <- unname(cds)
usethis::use_data(xsl_datasets)
