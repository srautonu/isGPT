#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
library("reshape2")

###### cumFeatureImpAll.eps ###################

# Data for different feature types (All features)
data = data.frame(`Category` = character(), CumImp = numeric());
data = rbind(data, data.frame(`Category` = "PSN"    , CumImp = -0.0008415906));
data = rbind(data, data.frame(`Category` = "n-grams", CumImp = -0.0009376378));
data = rbind(data, data.frame(`Category` = "nGDip"  , CumImp = 0.03536461));

cumFeatureImpAll = ggplot(data,aes(x=`Category`, y = CumImp)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="none") +
  theme(aspect.ratio = 0.6) +
  geom_bar(aes(fill = Category),stat = "identity", position = "dodge", width = 0.5) +
  labs(x = "Feature Extraction Technique", y = "Cumulative Importance") + 

postscript(file = "cumFeatureImpAll.eps", paper = "letter");
cumFeatureImpAll;
dev.off();

###### cumFeatureImp3000.eps ###################

# Data for different feature types within top 3000 features
data = data.frame(`Category` = character(), CumImp = numeric());
data = rbind(data, data.frame(`Category` = "PSN"    , CumImp = 0.001006389));
data = rbind(data, data.frame(`Category` = "n-grams", CumImp = 0.02884622));
data = rbind(data, data.frame(`Category` = "nGDip"  , CumImp = 0.1139051));

cumFeatureImp3000 = ggplot(data,aes(x=`Category`, y = CumImp)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="none") +
  theme(aspect.ratio = 0.6) +
  geom_bar(aes(fill = Category),stat = "identity", position = "dodge", width = 0.5) +
  labs(x = "Feature Extraction Technique", y = "Cumulative Importance")

postscript(file = "cumFeatureImp3000.eps", paper = "letter");
cumFeatureImp3000;
dev.off();

###### featureSpacePerfCmp.eps ###################

# Data for different feature types within top 2500 features
data = data.frame(`Category` = character(), Accuracy = numeric(), MCC = numeric());
data = rbind(data, data.frame(`Category` = "PSN"    , Accuracy = 0.65, MCC = .30));
data = rbind(data, data.frame(`Category` = "n-grams", Accuracy = 0.86, MCC = .72));
data = rbind(data, data.frame(`Category` = "nGDip"  , Accuracy = 0.94, MCC = .87));
data = rbind(data, data.frame(`Category` = "COM"    , Accuracy = 0.95, MCC = .91));

df <- melt(data,  id.vars = "Category", variable.name = "Metric");

featureSpacePerfCmp = ggplot(df,aes(x=`Category`, y = value)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  theme(aspect.ratio = 0.6) +
  geom_bar(aes(fill = Metric),stat = "identity", position = "dodge") +
  labs(x = "Feature Extraction Technique", y = "Accuracy and MCC") +
  ylim(0,1)

postscript(file = "featureSpacePerfCmp.eps", paper = "letter");
featureSpacePerfCmp;
dev.off();

###### featureSpacePerfCmp2.eps ###################

# Data for different feature types each using 2500 features
data = data.frame(`Category` = character(), Accuracy = numeric(), MCC = numeric());
data = rbind(data, data.frame(`Category` = "PSN"    , Accuracy = 0.93, MCC = .86));
data = rbind(data, data.frame(`Category` = "n-grams", Accuracy = 0.93, MCC = .86));
data = rbind(data, data.frame(`Category` = "nGDip"  , Accuracy = 0.94, MCC = .88));
data = rbind(data, data.frame(`Category` = "COM"    , Accuracy = 0.95, MCC = .91));

df <- melt(data,  id.vars = "Category", variable.name = "Metric");

featureSpacePerfCmp = ggplot(df,aes(x=`Category`, y = value)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  theme(aspect.ratio = 0.6) +
  geom_bar(aes(fill = Metric),stat = "identity", position = "dodge") +
  labs(x = "Feature Extraction Technique", y = "Accuracy and MCC") +
  ylim(0,1) +
  coord_cartesian(ylim=c(0.80,1));

postscript(file = "featureSpacePerfCmp2.eps", paper = "letter");
featureSpacePerfCmp;
dev.off();

###### thresholdTuning.eps &  thresholdTuning_SMOTED.eps ###################

# Use the appropriate (SMOTE vs. Imbalanced) data here:
data = read.csv("thresholdTuning.csv");

df <- melt(data,  id.vars = "Threshold", variable.name = 'Metric');

thresholdTuning = ggplot(df,aes(x=Threshold,y=value)) +
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  geom_line(aes(colour=Metric),size =3) +
  labs(y = "Performance Score x 100");
  
postscript(file = "thresholdTuning.eps", paper = "letter");
thresholdTuning;
dev.off();
