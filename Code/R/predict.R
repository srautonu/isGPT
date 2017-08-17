library(e1071)
source('./filteredFeaturization.R');

svmFile        = "svm_classification_comb_SMOTED.rds";
sequenceFile   = "testingset.csv";
#featureFile    = "testFeaturized_comb.rds";
featureFile    = "";
predictionFile = "predicted.csv";

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Reading sequence file from", sequenceFile, "...\n");
data = read.csv(sequenceFile);
cat(as.character(Sys.time()),">> Done.\n");

if (file.exists(featureFile)) {
  cat(as.character(Sys.time()),">> Reading features from", featureFile, "...\n");
  querySet = readRDS(featureFile);
  querySet = featurefiltering(querySet, colnames(svmmodel$SV));
} else {
  cat(as.character(Sys.time()),">> Generating features ...\n");
  querySet = filteredFeaturization(data$Sequence, colnames(svmmodel$SV));
  saveRDS(querySet, featureFile);
}
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Predicting subGolgi protein type ...\n");
svmpred = predict(svmmodel, querySet);

write.csv(svmpred, predictionFile);
cat(as.character(Sys.time()),">> Done. Predictions saved to", predictionFile, "\n");
