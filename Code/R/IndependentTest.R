library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');

timestamp();

#set.seed(10);

DoRegression    = TRUE;
svmC            = 10;
maxFeatureCount = 2800; # 2650, 2500

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

# For regression study, we need to 'unfactor' the dependent var.
#
if (DoRegression) {
  
  # Cis-Golgi becomes 1 and Trans-Golgi becomes 2.
  # But we want Cis-Golgi (positive class) to be 1 and Trans-Golgi to be 0
  features$protection = 2 - as.numeric(features$protection);
  testFeatures$protection  = 2 - as.numeric(testFeatures$protection);
}

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);

model = svm(protection ~ ., trainingSet, cost = svmC, kernel="linear");
pred = predict(model, testSet);

if (DoRegression) {
  # perform regression based perf. measurements

  # Find optimal threshold based on accuracy
  # Also find the AUCROC
  predAndTruth = prediction(pred, testSet$protection);
  auc  = ROCR::performance(predAndTruth,"auc")@y.values[[1]];
  
  accSeries = ROCR::performance(predAndTruth,"acc");
  threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];
  #threshold = 0.5;
  
  cat("AUCROC     : ", auc, "\n");
  cat("Threshold  : ", threshold, "\n");
  
  predAndTruth = prediction(as.numeric(pred >= threshold), testSet$protection);
} else {
  predAndTruth = prediction(as.numeric(pred), as.numeric(testSet$protection));
}
  
acc  = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2]
sens = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
spec = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
mcc  = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];

if (DoRegression) {
  # For regression we set Cis-Golgi to 1. So, sens is accuracy of Cis-Golgi
  # Let's swap the sens and spec
  temp = sens;
  sens = spec;
  spec = temp;
}

cat("Accuracy (Overall)    : ", acc, "\n");
cat("Accuracy (Trans-Golgi): ", sens, "\n");
cat("Accuracy (Cis-Golgi)  : ", spec, "\n")
cat("MCC                   : ", mcc, "\n")
