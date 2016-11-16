library(lattice) 
library(corrplot)
graphics.off()									# Removes all previously plotted graphs
cat("\014")                  							# Clears screen
rm(list=ls())									# Clears environment																

setwd(getwd())									# Changed working directory
# Reading CSV File
mydataset <- as.data.frame(read.csv(file="home_data.csv", header=T, sep=","))				
																			


clean <- function(x){								# Create a function for dataset cleaning
 suppressWarnings(as.numeric( gsub('[^a-zA-Z0-9.]', '', x)))			# Remove all unneccessary symbols and convert into numeric	
 }
  mydataset[] <- sapply(mydataset, clean)					# Call function using sapply on dataset

mydatasetcla <- subset( mydataset, select = -c(id, date ) )			# Removing House id and date


for(i in 2:ncol(mydatasetcla)){							# Scatter Plots
plot(mydatasetcla[,i],mydatasetcla$price, ylab = "Price", xlab = colnames(mydatasetcla)[i])
}
																											
cor(mydatasetcla)[1,]								# Correlation matrix for Price

#Spliting into training and test data

ind<-sample(2,nrow(mydatasetcla),replace=TRUE,prob=c(0.85,0.15))
training_data<-mydatasetcla[ind==1,]
test_data<-	mydatasetcla[ind==2,]

#Creating a linear model
linearModel <- lm(formula = price ~., data = training_data)
summary(linearModel)								#Prints summary of Linear Model

#Updating Linear Model to eliminate unncessary variables
linearModel <- update(linearModel, .~.-sqft_basement-floors-sqft_lot-sqft_lot15-yr_renovated)
summary(linearModel)
														
pred <- predict(linearModel,test_data)						# Prediction of prices using linear model

actuals_preds <- data.frame(cbind(actuals=test_data$price, predicteds=pred))	# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
print(actuals_preds)								# Comparision of actual and predicted values

plot(pred, test_data$price)							# Plot of actual vs predicted values		

#Diagnosis of the model
plot(linearModel)								# Plot.lm(linearModel)

plot(residuals(linearModel))							# Plots residuals

