library(e1071)
library(ROCR)
library(randomForest)

source('featurization.R');
source('featurefiltering.R');

timestamp();

rngSeed = 10;
fScheme = "_posTrimer";

amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

nTrainingSet = read.csv('trainingset.csv')
nTestSet = read.csv('testingset.csv')
cat(as.character(Sys.time()),">> Training set entries:", length(nTrainingSet[,1]), "\n");
cat(as.character(Sys.time()),">> Test set entries:", length(nTestSet[,1]), "\n");

nTrain = length(nTrainingSet[,1])
nTest = length(nTestSet[,1])

# File names #
rfmodelFile = paste("rfmodel_", as.character(nTrain), fScheme, ".rds", sep = "");
svmFile     = paste("svm_", as.character(nTrain), fScheme, ".rds", sep = "");
featureFile = paste("featurized1_", as.character(nTrain), fScheme, ".rds", sep = "");
testFile = paste("testFile1_", as.character(nTest), fScheme, ".rds", sep = "");
outFile     = paste("out_", as.character(nTrain), fScheme, ".csv", sep = "");
lcFile      = paste("lc_", as.character(nTrain), fScheme, ".csv", sep = "");     

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  alldata = rbind(nTrainingSet, nTestSet)
  featurizeddata = featurization(alldata$Sequence, alldata$Class, amins, seqorder = 3, gap = 15, posorder = 3);
  features = featurizeddata[1:nTrain,]; 
  testFeatures = featurizeddata[(nTrain+1):(nTrain+nTest),]; 
  saveRDS(features, featureFile);
  saveRDS(testFeatures, testFile);
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
  testFeatures = readRDS(testFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", testFile, ")\n");
  #features$protection = as.numeric(unlist(lapply(features$protection,function(x){if(x=="Trans-Golgi"){1}else{-1}})))
  #testFeatures$protection = as.numeric(unlist(lapply(testFeatures$protection,function(x){if(x=="Trans-Golgi"){1}else{-1}})))
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

cat(as.character(Sys.time()),">> Computing random forest ...\n");
if (!file.exists(rfmodelFile)) {
  rfmodel = randomForest(protection ~ ., features[1:nTrain,], importance=TRUE);
  saveRDS(rfmodel, rfmodelFile);
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  rfmodel = readRDS(rfmodelFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", rfmodelFile, ")\n");
}

rankedFeatures = rownames(rfmodel$importance[order(-rfmodel$importance[,3]),])

cat(as.character(Sys.time()),">> Calculating test features ... ","\n")
