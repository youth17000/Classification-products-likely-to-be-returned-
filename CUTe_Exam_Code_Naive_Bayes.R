# Clear the Global Environment
rm(list=ls(all=TRUE))

# Set working directory
setwd("E:\\Data Science\\INSOFE\\CUTE\\CUTE 2")
dir()

# Import Libraries
library(DMwR)
library(MASS)
dev.off()

# Read the data from csv file
data <- read.csv("Data_20171121.csv", header = TRUE, sep = ",")

# Structure of data
str(data)
summary(data)
names(data)

###############################################################################
###################~~~~~~~~~~~~~~DATA PREPROCESSING~~~~~~~~~~~~################
###############################################################################


# Checking for any missing values
sum(is.na(data))

# Removing the ID and S7 columns
# S7 column contains only "27" as a numeric value thoughout and does not add any information
data <- subset(data, select = -c(ID, S7))
names(data)

# Substituting "-99" as "NA"
# As per the problem desription "-99" is considered as missing value i.e. "NA"
data$P6[data$P6 == -99] <- NA
data$P12[data$P12 == -99] <- NA
str(data)

# Imputing missing values
data_imputed <- knnImputation(data, k = 10) # KNN Imputation
sum(is.na(data_imputed))
data_imputed

# Checking the data after imputation
str(data_imputed)
summary(data_imputed)

#Making factors of categorical variable
data_imputed[, c("PI","RF1","RF2","RF3","RF4","RF5","Target")] =
  lapply(data_imputed[, c("PI","RF1","RF2","RF3","RF4","RF5","Target")],as.character)

data_imputed[, c("PI","RF1","RF2","RF3","RF4","RF5","Target")] =
  lapply(data_imputed[, c("PI","RF1","RF2","RF3","RF4","RF5","Target")],as.factor)
str(data_imputed)

# Splitting the data into categorical and numerical data to pre-process it seperately
data_imputed_num <- data_imputed[, -c(13,17:22)]
data_imputed_cat <- data_imputed[, c(13,17:22)]


############################################################################
# Binning the numerical features to form categories based on the "quantiles"
############################################################################
data_imputed_num <- apply(data_imputed_num, MARGIN = 2, function(x){
                             .bincode(as.matrix(x), 
                             breaks = quantile(as.matrix(x), seq(0, 1, by = 0.1)),
                             include.lowest = TRUE)})

# Binding the numerical and categorical features together
data_imputed <- data.frame(cbind(data_imputed_num, data_imputed_cat))
str(data_imputed)

# Converting numerical binned features into categories
data_imputed[, 1:15] = lapply(data_imputed[, 1:15],as.character)
data_imputed[, 1:15] = lapply(data_imputed[, 1:15],as.factor)

# Checking the final structure of data before splitting into train-test
str(data_imputed)


#create data partition
set.seed(123)
datarows<-createDataPartition(data_imputed$Target,times = 1,p = 0.7,list = F)
datatrain<-data_imputed[datarows,]
datatest<-data_imputed[-datarows,]

######################################################################################
#####################~~~~~~~~~~~Building NAIVE BAYES Model~~~~~~~~~~~~~###############
######################################################################################
library(caret)
library(e1071)
model = naiveBayes(datatrain$Target ~ ., data = datatrain)
model

# Train Predictions
pred = predict(model, datatrain)
confusionMatrix(pred, datatrain$Target)

# Test Predictions
pred = predict(model, datatest)
confusionMatrix(pred, datatest$Target)
