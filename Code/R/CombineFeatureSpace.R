source('./combineFeatures.R');

timestamp();

balancing = "_SMOTED";
fScheme   = "_comb_pseAAC";

fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");

fSubSchemes = c("_pseAAC", "_PSF","_nGrams", "_nGDip");

# Generate combined feature ranking
cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
if (!file.exists(rankedFeaturesFile)) {
  combFeatureNames = c();
  combFeatureScores = c();
  for (fSubScheme in fSubSchemes) {
    curRFmodelFile = paste("rfmodel", fSubScheme, balancing, ".rds", sep = "");
    curRFModel = readRDS(curRFmodelFile);
    cat(as.character(Sys.time()),">> Read file:", curRFmodelFile, "\n");

    curScores = curRFModel$importance[,3];
    combFeatureScores = c(combFeatureScores, curScores);
    combFeatureNames = c(combFeatureNames, rownames(curScores));
  }

  saveRDS(combFeatureNames[order(-combFeatureScores)], rankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done. Saved to", rankedFeaturesFile, "\n");

} else {
  rankedFeatures = readRDS(rankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");
}

# Generate combined feature vectors for training and test sets

cat(as.character(Sys.time()),">> Combining Training set features ...\n");
if (!file.exists(featureFile)) {
  features = combineFeatures("featurized", fSubSchemes, balancing);
  saveRDS(features, featureFile);
  cat(as.character(Sys.time()),">> Done. Saved to ", featureFile, "\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done. (From cach file ", featureFile, ")\n");
}

cat(as.character(Sys.time()),">> Combining Test set features ...\n");
if (!file.exists(testFeatureFile)) {
  testFeatures = combineFeatures("testFeaturized", fSubSchemes, "");
  saveRDS(testFeatures, testFeatureFile);
  cat(as.character(Sys.time()),">> Done. Saved to ", testFeatureFile, "\n");
} else {
  testFeatures = readRDS(testFeatureFile);
  cat(as.character(Sys.time()),">> Done. (From cach file ", testFeatureFile, ")\n");
}