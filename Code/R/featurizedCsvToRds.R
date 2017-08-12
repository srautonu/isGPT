# featureFileNames =  c(
#   "featurized_nGDip_SMOTED",
#   "featurized_nGrams_SMOTED",
#   "featurized_PSF_SMOTED",
#   "featurized_comb_SMOTED"
# );

featureFileNames =  c(
  "featurized_pseAAC_SMOTED"
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