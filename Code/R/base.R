library(e1071)
library(ROCR)
library(randomForest)

source('featurization.R');
source('featurefiltering.R');

timestamp();

set.seed(10);

balancing = "_SMOTED";
fScheme   = "_comb_pseAAC";

amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

nTrainingSet = read.csv('trainingset.csv')
nTestSet = read.csv('testingset.csv')
cat(as.character(Sys.time()),">> Training set entries:", length(nTrainingSet[,1]), "\n");
cat(as.character(Sys.time()),">> Test set entries:", length(nTestSet[,1]), "\n");

nTrain = length(nTrainingSet[,1])
nTest = length(nTestSet[,1])

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");
svmFile            = paste("svm"           , fileNameSuffix, sep = "");
rfmodelFile        = paste("rfmodel"       , fileNameSuffix, sep = "");

outFile            = paste("out", fScheme, balancing, ".csv", sep = "");

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  alldata = rbind(nTrainingSet, nTestSet)
  featurizeddata = featurization(alldata$Sequence, alldata$Class, amins, seqorder = 3, gap = 15, posorder = 3);
  features = featurizeddata[1:nTrain,]; 
  testFeatures = featurizeddata[(nTrain+1):(nTrain+nTest),];
  saveRDS(features, featureFile);
  saveRDS(testFeatures, testFeatureFile);
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
  testFeatures = readRDS(testFeatureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", testFeatureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

if (!file.exists(rankedFeaturesFile)) {
  cat(as.character(Sys.time()),">> Computing random forest ...\n");
  if (!file.exists(rfmodelFile)) {
    rfmodel = randomForest(protection ~ ., features[1:nTrain,], importance=TRUE);
    saveRDS(rfmodel, rfmodelFile);
    cat(as.character(Sys.time()),">> Done.\n");
  } else {
    rfmodel = readRDS(rfmodelFile);
    cat(as.character(Sys.time()),">> Done ( from cached file:", rfmodelFile, ")\n");
  }
  
  cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
  rankedFeatures = rownames(rfmodel$importance[order(-rfmodel$importance[,3]),])
  cat(as.character(Sys.time()),">> Done\n");

} else {
  cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
  rankedFeatures = readRDS(rankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");
}
