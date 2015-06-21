removeUnnecessaryColumns <- function(old_pml) {
  #first, remove columns that are mostly null or blank
  b <- 1; i <- 2;
  #sapply(pml[,b], function(b) { print(paste(b, names(pml)[b], class(pml[,b]),fractionNA(pml[,b]))});
  
  new_pml <- data.frame(old_pml[,1])
  names(new_pml) <- names(old_pml[1])
  
  for(b in 2:160) { if(fractionEmpty(old_pml[,b]) < .5) { new_pml <- cbind(new_pml, old_pml[,b]);}}
  for(b in 2:160) { if(fractionEmpty(old_pml[,b]) < .5) { names(new_pml)[i] <- names(old_pml)[b]; i <- i + 1}}
  
  
  #remove columns that are not relevant, like timestamps
  names(new_pml)
  cnames <- names(new_pml); #just to make the next line a little shorter
  to_remove <- c(grep("X", cnames), grep("user_name",cnames), grep("num_window", cnames), 
                 grep("raw_timestamp_part_1", cnames), grep("raw_timestamp_part_2", cnames), 
                 grep("cvtd_timestamp", cnames), grep("new_window",cnames));
  new_pml <- new_pml[-to_remove]
  
  #removing the timestamps because they don't seem to be relevant
  #removing new_window because almost 98% are no and it does not seem to convey useful information
  #head(new_pml);
  
  #now randomize the order so we don't have all the same type if we do k-fold cross validation (should not be necessary but shouldn't hurt)
  new_indices <- sample(1:dim(new_pml)[1], replace=FALSE)
  new_pml <- new_pml[new_indices,];
  return(new_pml);
}

fractionNA <- function(x) {
  sum(is.na(x)) / length(x);
}

fractionBlank <- function(x) {
  sum(x == "") / length(x);
}

fractionEmpty <- function(x) {
  empty <- sum(is.na(x));
  empty <- empty + sum(x == "", na.rm=TRUE);
  empty / length(x);
}

#pml_write_files was given in the assignment description; not written by me
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(as.character(x[i]),file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE,eol='')
  }
}

doKFoldTraining <- function(pml, folds) {
  models <- vector();
  for(i in 1:folds) {
    models <- c(models, doOneFold(pml, folds, i));
  }
  for(i in 1:length(models)) {
    #validate each one
  }
  return(models);
}

doOneFold <- function(pml, folds, fold) {
  inTrain <- getFoldIndices(pml, folds, fold);
  pml_train <- pml[inTrain,];
  pml_validate <- pml[-inTrain,];
  rfModel <- train(classe ~ ., data=pml_train, method="rf");
  rfModel;
  rfModel$finalModel;
  rfValidation <- predict(rfModel, pml_validate);
  rfValidation;
}

getFoldIndices <- function(pml, folds, fold) {
  #TODO: check that fold is a legit number
  
  indicesPerFold <- dim(pml)[1] / folds;
  start <- indicesPerFold * fold;
  end <- start + indicesPerFold;
  return(start:end);
}