# ======================================================
#  Weight Lifting, Human Activity Recognition (HAR)
#  Machine Learning Project, Johns Hopkins
#  Peer-graded Assignment: Prediction Assignment Writeup
# ======================================================

## ============================================
## Load Test and Training Set Data
## ============================================

## Load Training Set Data
training <- read.csv("../data/pml-training.csv")

## Load Testing Set Data
testing <- read.csv("../data/pml-testing.csv")

## ============================================
## Exploratory Analysis
## ============================================

## Explore set dimensions
dim(training)
dim(testing )

# Identify set column name differences
trainCol <- names(training)
testCol <- names(testing)
setdiff(trainCol, testCol)
setdiff(testCol, trainCol)

# Assess numbers in each class
table(training$classe)

# Examine grouping of test data
testing$problem_id

## Get 6 user names
user <- unique(sort(training$user_name))

## Compare class of objects ??Use??
trainClass <- sapply(training, class)
testClass <- sapply(testing, class)
setdiff(testClass, trainClass)

## ============================================
## Clean and Partition Data
## ============================================

# Create function to assess number of NAs in data sets
na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

# Identify columns that do no have primarily NA values
boolNotNAs <- na_count(training) < 19000
rm(na_count)

# Subset test and training sets to filter out 
# primarily NA columns 
train2 <- training[,boolNotNAs]
test2 <- testing[,boolNotNAs]
rm(boolNotNAs,training, testing)

# Verify set column name differences ??use??
trainCol <- names(train2)
testCol <- names(test2)
setdiff(trainCol, testCol)
setdiff(testCol, trainCol)
rm(trainCol, testCol)

# Create function to assess number of blank
# values in factor columns
empty_count <-function (x) sapply(x, function(y) sum(y == ""))

# Identify columns that do not have primarily
# blank factor contents
boolNotEmpty <- empty_count(train2) < 19000
rm(empty_count)

# Subset test and training sets to filter out 
#  columns with primarily blank factor contents
train3 <- train2[,boolNotEmpty]
test3 <- test2[,boolNotEmpty]
rm(boolNotEmpty,train2, test2)

# Filter out index, timeseries, and windowing columns
train4 <- train3[,-c(1,3:7)] 
test4 <- test3[,-c(1,3:7)] 
rm(test3,train3)

# Load caret for data partitioning
library(caret)

# Create validation test set
inTrain <- createDataPartition(train4$classe, p=0.7, list = F)
train4 <- train4[inTrain,]
valid4 <- train4[-inTrain,]
rm(inTrain)

# trainClass <- sapply(train3, class)

## ============================================
## Train Model and Validate Preditions
## ============================================
# Load caret, randomForest
# library(randomForest)

ctrl <- trainControl(method = "oob")

set.seed(1235)

# Train model using Random Forest
modFit <- train(classe ~ ., data = train4, method = "rf", prox = T, importance = T, trControl = ctrl)



## ============================================
## Predictions on Test Set and Findings
## ============================================



