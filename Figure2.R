
library(forestplot)

# Input results in data frame
sens_results <- as.data.frame(1:11)
sens_results$index <- 1:11
sens_results$analysis = c( "MonsoonYes","MonsoonNo","NoBuffer","Age24+","Age18-24",
                           "ExposureThresh10%", "ExposureThresh2%", "ExposureQ4","ExposureQ3","ExposureQ2","Overall-IPTW")
sens_results <- drop_columns(sens_results, 1)
sens_results$PR <- c(1.11, #IPTW, EQ2
                1.337,1.53,#opv overall, 1, 2
                1.32,1.29,1.12,#DTP overall, 1 , 2
                1.38, 1.32, 1.30, 1.45, 1.22)
sens_results$PR <- c(1.32, 1.29,#monsoonyes, no
                     1.12,#no buf
                    1.32,1.38, #Age 24, age 18
                     1.45,1.30,#exposure 10, 2  
                    1.53,1.337,1.11, #exposure 4,3,2
                      1.22)#overall
sens_results$LowerCI <- c(1.11, 1.11,
                          0.99,
                     1.14,1.14,
                     1.26, 1.15,
                     1.28, 1.13, 0.92, 
                     1.06)
sens_results$UpperCI <- c(1.56,1.50,
                     1.28,
                     1.52,1.67,
                     1.66,1.46,
                     1.82, 1.64, 1.35,
                     1.45)

# Creating the forest plot

color = c("black","black","black","black","black","black","black","black","black", "black","red")
ggplot_sens <- ggplot(data=sens_results, aes(y=index, x=PR, xmin=LowerCI, xmax=UpperCI)) +
  geom_point(color=color) + 
  geom_errorbarh(height=.1, color=color) +
  scale_y_continuous(breaks=1:nrow(sens_results), labels=sens_results$analysis) +
  labs(title='Figure 2. Analysis Results', x='Prevalence Ratio', y = 'Analysis') +
  xlim(0.5,2.2)+
 geom_vline(xintercept=1, color='black', linetype='dashed', alpha=1) +
 geom_hline(yintercept=10.5, color="black", linetype='solid', alpha=4)+
  theme_cowplot()

ggplot_sens
