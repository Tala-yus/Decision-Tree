# Machine Learning Project 

library(C50)
library(ggplot2)
library(knitr)

dataset <- read.csv("C:/Users/Lenovo/Downloads/DiabetesData-1.csv")


#Convert the dataset to a dataframe 
set <- as.data.frame(dataset) 

View(set)

colnames(set)[ncol(set)] <- "Diabetic"


x <- sum(set$Diabetic == "0")

y <- sum(set$Diabetic == "1")

# summary for 1
summ <- summary(set)
kable(summ)

# plot for 2
bars <- data.frame(Diabetic = c("0", "1"), count = c(x, y))


ggplot(data = bars, aes(x=Diabetic, y=count)) +
    geom_bar(stat="identity", fill = "steelblue", width = 0.7) +
    geom_text(aes(label = count), vjust = 0.1, size = 20) +
    theme_minimal()+
     theme(axis.text = element_text(size = 40), axis.title = element_text(size = 14, face = "bold")) 



percX = x / nrow(set) * 100
percX
percY = y / nrow(set) * 100    
percY


# Model 1 70% percent training 30% test data
set$Diabetic <- factor(set$Diabetic)

set.seed(123) #to generate the same randomly selected rows in any run
trainrandom <- sample(1:nrow(set), 0.7 * nrow(set)) # 70% randomly selected 
trainData <- set[trainrandom, ] 
testData <- set[-trainrandom, ] #exclude the trainning data from the set 


View(trainData)
View(testData)


modelOne <- C5.0(trainData[ , -c(1,8,9)], trainData[, 9])

result <- predict(modelOne, testData[, -c(1,8,9)])

View(cbind(testData, result))

temp <- result == testData$Diabetic

accuracy <- length(which(temp)) / length(temp) * 100.0

sprintf("Accuracy is = %.2f", accuracy)

summary(modelOne)

plot(modelOne, type = "s", main = "Model One Decision Tree")

plot(modelOne, main = "Model One Decision Tree")

############################################################

# Model 2 50% Training 50% test

set.seed(2241) #to generate the same randomly selected rows in any run
trainrandom <- sample(1:nrow(set), 0.5 * nrow(set)) # 50% randomly selected
trainData <- set[trainrandom, ]
testData <- set[-trainrandom, ] #exclude the trainning data from the set 

View(trainData)

View(testData)

modelTwo <- C5.0(trainData[ , -c(1,8,9)], trainData[, 9])

result <- predict(modelTwo, testData[ , -c(1,8,9)])

temp <- result == testData$Diabetic

accuracy <- length(which(temp)) / length(temp) * 100.0

sprintf("Accuracy is = %.2f", accuracy)

summary(modelTwo)

plot(modelTwo, type = "s", main = "Model Two Decision Tree")
