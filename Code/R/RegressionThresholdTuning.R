library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');

timestamp();

#set.seed(10);

svmC            = 100;
maxFeatureCount = 2250;

balancing = "";
fScheme   = "_comb";

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile  = "rankedFeatures.rds";
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");

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

# Cis-Golgi becomes 1 and Trans-Golgi becomes 2.
# But we want Cis-Golgi (positive class) to be 1 and Trans-Golgi to be 0
features$protection = 2 - as.numeric(features$protection);
testFeatures$protection  = 2 - as.numeric(testFeatures$protection);

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);

model = svm(protection ~ ., trainingSet, cost = svmC, kernel="linear");
pred = predict(model, testSet);

# perform regression based perf. measurements

# Find optimal threshold based on accuracy
# Also find the AUCROC
predAndTruth = prediction(pred, testSet$protection);
auc  = ROCR::performance(predAndTruth,"auc")@y.values[[1]];

cat(as.character(Sys.time()),">> Recording performance in results.csv ...\n");
itData = data.frame(matrix(ncol = 4, nrow = 0))
for (threshold in seq(from=0.1, to=.99, by=0.01)) {
  
  predAndTruth = prediction(as.numeric(pred >= threshold), as.factor(testSet$protection));
  acc  = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
  mcc = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];
  
  itData = rbind(itData, c(threshold, acc, sens, spec, mcc));
}
colnames(itData) = c("Threshold", "Accuracy", "Sensitivity", "Specificity", "MCC");
write.csv(t(itData), "results.csv");
cat(as.character(Sys.time()),">> Done.\n");
