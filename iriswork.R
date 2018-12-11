library("caret")
library("ellipse")

data("iris")
dataset <- iris

validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

cat("\n\nDimensions of dataset\n")
dim(dataset)
cat("\n\nDimensions of iris\n")
dim(iris)
cat("\n\nDimensions of validation dataset\n")
dim(validation)
cat("\n\nDimensions of training/testing dataset\n")
dim(validation_index)

cat("\n\nLooking at columns of dataset and their classes\n")
sapply(dataset, class)
cat("\n\nLooking at head of dataset\n")
head(dataset)
cat("\n\nLooking at classes in Species\n")
levels(dataset$Species)

cat("\n\nLooking at frequency of classes within Species\n")
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

cat("\n\nLooking at summary of dataset\n")
summary(dataset)

cat("\n\nBasic Visualizations For Understanding Data:\n")

cat("\n\nBoxplot for each attribute (one image)\n")
x <- dataset[,1:4]
y <- dataset[,5]
par(mfrow = c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main = names(iris)[i])
}

cat("\n\nBarplot for class breakdown\n")
plot(y)

cat("\n\nScatterplot matrix\n")
featurePlot(x = x, y = y, plot = "ellipse")
