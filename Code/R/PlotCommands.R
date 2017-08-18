# Plotting the threshold tunin curve
# Assuming the generated data is in variable called itData
df <- melt(itData,  id.vars = "Threshold", variable.name = 'Metric');
myplot = ggplot(df,aes(x=Threshold,y=value));
myplot = myplot + geom_line(aes(colour=Metric),size =1.5);
myplot = myplot + theme_bw();
myplot = myplot + theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank()
                        );
myplot;

