
# Creating dataset with all of the results 

library(forestplot)

results <- as.data.frame(1:9)
results$index <- 1:9
results$vaccine = c("nFIC", "BCG", 
                    "OPV2","OPV1","OPV",
                    "DPT2","DPT1","DPT",
                    "MCV")
results <- drop_columns(results, 1)
results$PR <- c(1.22,1.41, #nfic, bcg
                1.33,1.50,1.27,#opv overall, 1, 2
                1.27,1.43,1.25,#DTP overall, 1 , 2
                1.24)
results$LowerCI <- c(1.06,1.23,
                     1.14,1.24,1.10,
                     1.09,1.21,1.09,
                     1.06)
results$UpperCI <- c(1.45,1.62,
                     1.56,1.80,1.45,
                     1.49,1.69,1.43,
                     1.45)
#results$CI <- paste(results$LowerCI,results$UpperCI,sep=",")
results$pvalue <- c(0.005, "<0.001", 
                    "<0.001","<0.001","<0.001",
                    "<0.001","<0.001", 0.002,
                    0.006)



color = c("red","black","skyblue","skyblue","black","skyblue","skyblue","black","black")
ggplot_forest_subvac <- ggplot(data=results, aes(y=index, x=PR, xmin=LowerCI, xmax=UpperCI)) +
  geom_point(color=color) + 
  geom_errorbarh(height=.1, color=color) +
  scale_y_continuous(breaks=1:nrow(results), labels=results$vaccine) +
  labs(title='Figure S2. Prevalence Ratio by Vaccine and Dose', x='PR', y = 'Vaccine') +
  xlim(0.5,2.2)+
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=1) +
  theme_cowplot()

ggplot_forest_subvac