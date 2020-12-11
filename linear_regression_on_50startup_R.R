
startup_data <- read_csv("50_Startups.csv")

head(startup_data)
summary(startup_data)

par(mfrow = c(1,3))
boxplot(startup_data$`R&D Spend`)
boxplot(startup_data$Administration)
boxplot(startup_data$`Marketing Spend`)


library(caTools)

set.seed(10)

split_data <- sample.split(startup_data$Profit, SplitRatio = 0.75)

split_data

train_data <- subset(startup_data, split_data == T)
test_data <- subset(startup_data, split_data == F)


model <- lm(formula = Profit~., data = train_data)
model
summary(model)


prediction <- predict(model, test_data)
prediction

difference <- prediction - test_data$Profit

test_data <- cbind(test_data,prediction, difference)
View(test_data)

mape <- mean(abs(difference)/test_data$Profit)
mape
