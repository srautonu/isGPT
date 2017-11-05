featureFileNames =  c(
  "testFeaturized_pssm_nGDip25"
);


for (featureFileName in featureFileNames) {
  
  inputFileName  = paste(featureFileName, ".csv", sep="");
  outputFileName = paste(featureFileName, ".rds", sep="");

  cat(as.character(Sys.time()),">> Reading file", inputFileName, " ...\n");
  t = read.csv(inputFileName);
  cat(as.character(Sys.time()),">> Done.\n");
  
  cat(as.character(Sys.time()),">> Writing file", outputFileName, " ...\n");
  saveRDS(t, outputFileName);
  cat(as.character(Sys.time()),">> Done.\n");
}