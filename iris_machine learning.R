data("iris")
df1<- data.frame(iris)
str(df1)
head(df1)
library(caret)
validation_index<- createDataPartition(df1$Species, p=0.80, list=FALSE)
#select 20% data for validation
validation<- df1[-validation_index,]
#use the 80% of data for training and testing the model
df2<- df1[validation_index,]
dim(df2)
#list types of each attribute
sapply(df2, class)
head(df2)
#list how many levels of the class
levels(df2$Species)
summary(df2)


#now we head to visualization for better understanding
#we start by univariate plots(i.e plot for each variable)

x<- df2[,2:16]
y<- df2[,1]
#given that all input variable are numeric we can create box plots and whisker plots
#box plots for each attribute in one image
par(mfrow= c(1,4))
for (i in 1:4){
  boxplot(x[,i], main=names(df2)[i])
}

plot(y)


#now we head to multivariate analysis to identify relationships between varibales
#this will help out to tease out linear seperation between the classes
library(ggplot2)
library(ISLR)
#box and whisker plots
featurePlot(x=x, y=y, plot ="box")

#density plots for each attribute by class value

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x , y=y, plot = "density" , scales=scales)


#10 fold cross validation to estimate accuracy
control <- trainControl(method = "cv" , number = 10)
metric <- "Accuracy"


#lets build our model
#linear algorithms
set.seed(7)
fit.lda<- train(Species~., data=df2, method="lda", metric=metric, trControl=control)

#nonlinear algorithms
#CART
set.seed(7)
fit.Cart<- train(Species~., data=df2, method="rpart", metric=metric, trContol=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=df2, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=df2, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=df2, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
