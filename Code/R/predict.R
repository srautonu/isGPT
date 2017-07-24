source('./filteredFeaturization.R');

svmFile = "SVM_Classification_NoCV_NoSMOTE.rds";
sequenceFile = "testingset.csv";
featureFile = "testingset.rds"

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Reading sequence file from", sequenceFile, "...\n");
data = read.csv(sequenceFile);
cat(as.character(Sys.time()),">> Done.\n");

if (file.exists(featureFile)) {
  cat(as.character(Sys.time()),">> Reading features from", featureFile, "...\n");
  querySet = readRDS(featureFile);
} else {
  cat(as.character(Sys.time()),">> Generating features ...\n");
  querySet = filteredFeaturization(data$Sequence, colnames(svmmodel$SV));
  saveRDS(querySet, featureFile);
}
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Predicting subGolgi protein type ...\n");
svmpred = predict(svmmodel, querySet);
saveRDS(svmpred, "predictions.rds");
cat(as.character(Sys.time()),">> Done.\n");
