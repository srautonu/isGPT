library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');

timestamp();

#set.seed(10);

balancing = "_SMOTED";
fScheme   = "_comb";

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile  = "rankedFeatures.rds";
#rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");
svmFile            = paste("svm"           , fileNameSuffix, sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading test set features from", testFeatureFile, "...\n");
testFeatures = readRDS(testFeatureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

# random shuffle of features
features <- features[sample(nrow(features)),]

svmC = 10;
maxFeatureCount = 2800; 

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features     = featurefiltering(features, rankedFeatures, max(featureCountList));
testFeatures = featurefiltering(testFeatures, rankedFeatures, max(featureCountList));

trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);

model = svm(protection ~ ., trainingSet, cost = svmC, kernel="linear");
pred = predict(model, testSet);
predAndTruth = prediction(as.numeric(pred), as.numeric(testSet$protection));
  
acc  = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2]
sens = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
spec = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
mcc  = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];

cat("Accuracy(Test set)   : ", acc, "\n");
cat("Sensitivity(Test set): ", sens, "\n");
cat("Specificity(Test set): ", spec, "\n")
cat("MCC(Test set)        : ", mcc, "\n")
