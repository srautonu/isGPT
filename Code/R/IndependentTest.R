library(e1071)
library(ROCR)

source('featurefiltering.R');

timestamp();

#set.seed(10);

DoRegression    = TRUE;

svmCostList = c(1);
featureCountList = c(2800); 

balancing = "_SMOTED";
fScheme   = "_comb";

accData = NULL;

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile  = "rankedFeatures.rds";
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");
outFile            = paste("out", fScheme, balancing, ".csv", sep = "");

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

features = featurefiltering(features, rankedFeatures, max(featureCountList));
testFeatures = featurefiltering(testFeatures, rankedFeatures, max(featureCountList));

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);
  
  for (svmC in svmCostList) 
  {
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
      predAndTruth = prediction(as.numeric(pred >= threshold), testSet$protection);
    } else {
      predAndTruth = prediction(as.numeric(pred), as.numeric(testSet$protection));
    }
  
    acc  = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2]
    sens = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
    spec = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
    mcc  = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];
  
    if (DoRegression) {
      cat(maxFeatureCount, ",", svmC, ",", auc,  ",", threshold, ",", acc, ",", spec, ",", sens, ",", mcc, "\n");
      accData = rbind(accData, c(maxFeatureCount, svmC, auc, threshold, acc, spec, sens, mcc));
    } else {
      
      cat(maxFeatureCount, ",", svmC, ",", acc, ",", sens, ",", spec, ",", mcc, "\n");
      accData = rbind(accData, c(maxFeatureCount, svmC, acc, sens, spec, mcc));
    }
  
    write.csv(accData, outFile);
  }
}