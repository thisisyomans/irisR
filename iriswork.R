library("caret")

data("iris")
dataset <- iris

validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

print("")
dim(dataset)
print("")
dim(iris)
print("")
dim(validation)
print("")
dim(validation_index)

print("")
sapply(dataset, class)