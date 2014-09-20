## Author: Brian von Konsky
## Created: 16 Sept 2014
## Course:  Practical Machine Learning

library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(randomForest)

# Returan the data frame used in this analysis
getData <- function(filename) {
  ## Set file names
  directory <- "data"
  
  # Set OS independentn paths to the files
  path.data<- file.path(directory, filename)
  
  # Read the data
  df <- read.csv(path.data, header=TRUE, stringsAsFactors=TRUE)
  
  # Clean the data
  keepCols <- c(8:11,37:49,60:68, 84:86,102, 113:124, 140, 151:159,160)
  df<- df[,keepCols]
  
  # Return the data
  return(df)
}

# get cleaned data
data.cleaned <- getData("pml-training.csv")
examination  <- getData("pml-testing.csv")

# Partition data
set.seed(1234)
inTraining    <- createDataPartition(data.cleaned$classe, p = 0.75, list = FALSE)
modelTraining <- data.cleaned[inTraining, ]
modelTesting  <- data.cleaned[-inTraining,]


# Use random forests to create a model
modelFit<- randomForest(classe ~ ., data=modelTraining)
modelFit

# check model with the partition set aside for testing
prediction <- predict(modelFit, modelTesting)

# report out-of-sample accuracy stats
cm <- confusionMatrix(prediction, modelTesting$classe)
cm$table
cm$overall[1]
cm$byClass

# write answers out to separate files for grading system
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# get answers for problemset and write them out
answers <- predict(modelFit, examination)
pml_write_files(answers)

