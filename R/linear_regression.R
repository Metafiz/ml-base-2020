data <- cars
?cars
str(data)
head(data)
summary(data)
plot(data$dist ~ data$speed)
hist(data$dist)

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed") 

par(mfrow=c(1, 2))  # разделение области графиков на 2 колонки
boxplot(cars$speed, main="Speed", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'


install.packages("e1071")
library(e1071)

par(mfrow=c(1, 2))  # разделение области графиков на 2 колонки
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Асимметрия:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Асимметрия:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

cor(cars$speed, cars$dist)

linearMod <- lm(dist ~ speed, data=cars)
linearMod <- lm(price ~ carat, data = diamonds)
linearMod

modelSummary <- summary(linearMod)
modelSummary

# коэффициенты детерминации
modelSummary$r.squared
modelSummary$adj.r.squared

# остаточная дисперсия
modelSummary$sigma ^ 2

library(broom)
glance(linearMod)

predictor_column <- "speed"
predictor_column <- "carat"
modelCoeffs <- modelSummary$coefficients  # коэффициенты модели
beta.estimate <- modelCoeffs[predictor_column, "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs[predictor_column, "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(linearMod)
BIC(linearMod)

# -- Проверка случайности остатков
linearMod$residuals
length(linearMod$residuals)

par(mfrow=c(1, 1))
plot(linearMod$fitted.values, linearMod$residuals,  
     xlab="Предсказанные значения", ylab="Значения остатков")

library(ggplot2)
qplot(y = linearMod$fitted.values, x = linearMod$residuals, 
      ylab="Предсказанные значения", xlab="Значения остатков")

# проверка на нулевое мат.ожидание остатков
M = mean(linearMod$residuals)
SD = sd(linearMod$residuals, na.rm = FALSE)
N = length(data$dist)
t_m <- -M / SD * sqrt(N)

pval = 0.95
t_tab <- qt(pval, N - 1)
if (abs(t_m) < t_tab) {
  print("Принимаем гипотезу H0 - мат.ожидание остатков равно нулю")
} else { 
  print("Принимаем гипотезу H1 с вероятностью ", pval, " - мат.ожидание остатков отлично от нуля")
}

# проверка гомоскедастичности
library(lmtest)
bptest(linearMod)

cor.test(data$speed, linearMod$residuals, method = "spearman")

# автокорреляция остатков
bgtest(linearMod)
dwtest(linearMod)

# проверка нормальности распределения остатков
qqnorm(linearMod$residuals)

hist(linearMod$residuals)
library(sm)
sm.density(linearMod$residuals, model = "Normal",
           xlab = "Resudual", ylab = "Функция плотности распределения")

# параметрические тесты
library(nortest)
lillie.test(linearMod$residuals)
shapiro.test(linearMod$residuals[1:4990])

# разбиение данных на обучающую и тестовую выборки
set.seed(100)
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
length(distPred)
summary (lmMod)

# вычисление точности прогнозирования
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

# кросс-валидация
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = cars, form.lm=dist ~ speed, m=5, 
                                   dots=FALSE, seed=29, legend.pos="topleft",  
                                   printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

library(caTools)
str(diamonds)
dmnds <- diamonds
split <- sample.split(dmnds$price, SplitRatio = 0.75)
train <- subset(dmnds, split == TRUE)
test <- subset(dmnds, split == FALSE)
