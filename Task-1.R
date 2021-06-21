#GRIP Task-1

#Import the required libraries
library(tidyverse)
library(ggpubr)

#Loading the data
sthrs <- read.delim("sthrs.txt",row.names=1)

#View the dataframe
View(sthrs)

#Description/Structure of the dataset
str(sthrs)

#Summary of the dataset
summary(sthrs)

print("Plotting the dataset")
ggplot(sthrs, aes(Hours, Scores)) +     
        geom_point() +
        stat_smooth()

print("Building the model")
sthrs_model <- lm(sthrs$Scores ~ sthrs$Hours)

#Plotting the regression model
ggplot(sthrs, aes(Hours, Scores)) +     
         geom_point() +
         stat_smooth(method = lm)

print("Model Summary")
summary(sthrs_model)

print("Check for outliers")

#Plotting the data for Residual Analysis
rs_plot <- plot(sthrs_model$fitted.values,rstandard(sthrs_model),
                 main = "Residual Plot",
                 xlab = "Predicted Scores",
                 ylab = "Standardized values")

#Setting the upper confidence limit
abline(h = 2,lty = 2)
 
#Setting the lower confidence limit
abline(h = -2, lty = 2)

print("Ientify the outliers")
identify(sthrs_model$fitted.values,rstandard(sthrs_model))

#Remove the outliers
 sthrs_new <- sthrs[-4,]
 
 print("Building new model after removing the outlier")
 sthrs_modelNew <- lm(sthrs_new$Scores ~ sthrs_new$Hours)
 
#Again, check for outliers
rs_plot <- plot(sthrs_modelNew$fitted.values,rstandard(sthrs_modelNew),
                 main = "Residual Plot",
                 xlab = "Predicted Values for Scores",
                 ylab = "Standardized Resiuals")
 abline(h = 2,lty = 2)
 abline(h = -2, lty = 2)
 
 print("No outlier is left")
 
 summary(sthrs_modelNew)
 
 #Observations:
 #RSE: Closer to zero the better
 #R-Squared: Higher the better
 #F-statistic: Higher the better

 print("Plotting the reduced model")
 ggplot(sthrs_new, aes(Hours, Scores)) +     
         geom_point() +
         stat_smooth(method = lm) 
 
#prediction using the above model
pred_value=(sthrs_modelNew$coefficients[[2]]*9.25)+sthrs_modelNew$coefficients[[1]]
 
print("Predicted score if a student studies for 9.25 hrs/day:")
print(pred_value)

