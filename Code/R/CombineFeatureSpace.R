source('./combineFeatures.R');

timestamp();

balancing = "";
fScheme   = "_comb_pseAAC";

fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");

fSubSchemes = c("_pseAAC", "_PSF","_nGrams", "_nGDip");

# Generate combined feature ranking
cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
if (!file.exists(rankedFeaturesFile)) {
  aacFeatures1 = c(
    "C_0_A", "C_0_C", "C_0_D", "C_0_E", "C_0_F", "C_0_G", "C_0_H", "C_0_I",
    "C_0_K", "C_0_L", "C_0_M", "C_0_N", "C_0_P", "C_0_Q", "C_0_R", "C_0_S",
    "C_0_T", "C_0_V", "C_0_W", "C_0_Y"
  )
  
  aacFeatures2 = c(
    "Xc1.A", "Xc1.C", "Xc1.D", "Xc1.E", "Xc1.F", "Xc1.G", "Xc1.H", "Xc1.I",
    "Xc1.K", "Xc1.L", "Xc1.M", "Xc1.N", "Xc1.P", "Xc1.Q", "Xc1.R", "Xc1.S",
    "Xc1.T", "Xc1.V", "Xc1.W", "Xc1.Y"
  )
  
  combFeatureNames = c();
  combFeatureScores = c();
  for (fSubScheme in fSubSchemes) {
    curRFmodelFile = paste("rfmodel", fSubScheme, balancing, ".rds", sep = "");
    curRFModel = readRDS(curRFmodelFile);
    cat(as.character(Sys.time()),">> Read file:", curRFmodelFile, "\n");
    
    curAccuracy = 1 - sum(curRFModel$confusion[,"class.error"])/2;
    combFeatureScores = c(combFeatureScores, curRFModel$importance[,3] * curAccuracy);
    combFeatureNames = c(combFeatureNames, rownames(curRFModel$importance));
  }
  
  rankedFeatures = combFeatureNames[order(-combFeatureScores)];
  # remove the duplicated (both times) AAC features
  # one set  from nGrams and one set from pseAAC
  rankedFeatures = rankedFeatures[!rankedFeatures %in% aacFeatures1];
  rankedFeatures = rankedFeatures[!rankedFeatures %in% aacFeatures2];
  
  # now add back one set of AAC (nGrams) features as highest priority
  rankedFeatures = c(aacFeatures1, rankedFeatures);
  saveRDS(rankedFeatures, rankedFeaturesFile);
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