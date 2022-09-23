
# Importing libraries

library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)


#Creating Dataset


total<-readRDS("Normal_total.Rds")
data<-total[1:1000,]
rm(total)
data<-data %>% select(CD8A,IFNG,PDLIM1,CTLA4,GZMK,CD4,CCR7,PERP,MAPK1,meta.vital_status)
data<-na.omit(data)


# Performs stratified random split of the data set


TrainingIndex <- createDataPartition(data$meta.vital_status, p=0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set
TestingSet <- data[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TrainSet$meta.vital_status<-as.factor(TrainSet$meta.vital_status)


# Building Random forest model


model <- randomForest(meta.vital_status ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)


# Save model to RDS file

saveRDS(model, "model.rds")
