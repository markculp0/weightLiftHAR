# ======================================================
#  Weight Lifting, Human Activity Recognition (HAR)
#  Machine Learning Project, Johns Hopkins
#  Peer-graded Assignment: Prediction Assignment Writeup
# ======================================================

## Load Training Set Data
train <- read.csv("../data/pml-training.csv")

## Get user names
user <- unique(sort(train$user_name))


