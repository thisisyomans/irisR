#install.packages("e1071", dependencies = TRUE) #run in console
#install.packages("caret") #run in console
#install.packages("ellipse") #run in console
library("e1071")
library("caret")
library("ellipse")

data("iris")
dataset <- iris

validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

cat("\nDimensions of dataset\n")
dim(dataset)
cat("\nDimensions of iris\n")
dim(iris)
cat("\nDimensions of validation dataset\n")
dim(validation)
cat("\nDimensions of training/testing dataset\n")
dim(validation_index)

cat("\nLooking at columns of dataset and their classes\n")
sapply(dataset, class)
cat("\nLooking at head of dataset\n")
head(dataset)
cat("\nLooking at classes in Species\n")
levels(dataset$Species)

cat("\nLooking at frequency of classes within Species\n")
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

cat("\nLooking at summary of dataset\n")
summary(dataset)

cat("\nBasic Visualizations For Understanding Data:\n")

cat("\nBoxplot for each attribute (one image)\n")
x <- dataset[,1:4]
y <- dataset[,5]
par(mfrow = c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main = names(iris)[i])
}

cat("\nBarplot for class breakdown\n")
plot(y)

cat("\nScatterplot matrix\n")
featurePlot(x = x, y = y, plot = "ellipse")

cat("\nBox and whisker plots for each attribute\n")
featurePlot(x = x, y = y, plot = "box")

cat("\nDensity plots for each attribute by class value")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

cat("\nRun algorithms using 10-fold cross validation\n")
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"


cat("\nModel Creation + Accuracy Tests\n")

cat("\na. Linear algorithms\n")

cat("\nLinear Discriminant Analysis (LDA)\n")
set.seed(7)
fit.lda <- train(Species~., data = dataset, method = "lda", metric = metric, trControl = control)

cat("\nb. Nonlinear algorithms\n")

cat("\nClassification and Regression Trees (CART)\n")
set.seed(7)
fit.cart <- train(Species~., data = dataset, method = "rpart", metric = metric, trControl = control)

cat("\nk-Nearest Neighbors\n")
set.seed(7)
fit.knn <- train(Species~., data = dataset, method = "knn", metric = metric, trControl = control)

cat("\nc. Advanced algorithms\n")

cat("\nSupport Vector Machines (SVM) w/ linear kernel\n")
set.seed(7)
fit.svm <- train(Species~., data = dataset, method = "svmRadial", metric = metric, trControl = control)

cat("\nRandom Forest (RF)\n")
set.seed(7)
fit.rf <- train(Species~., data = dataset, method = "rf", metric = metric, trControl = control)

cat("\nSummarize accuracy of all 5 models\n")
results <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

cat("\nCompare accuracy of all 5 models\n")
dotplot(results)

cat("\nSummarize best model\n")
print(fit.lda)
