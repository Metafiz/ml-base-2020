
# вызов справки по функциям
?read.table
?read.csv

# вывести рабочую директорию
getwd() 

# загрузка данных
mydata <- read.csv('rscripts/evals.csv')

# общие сведения
head(mydata, 3)
tail(mydata)

View(mydata)

# информация по типам данных полей
str(mydata)

# имена колонок
names(mydata)
a <- names(mydata)

# сводная статистика
summary(mydata)


# Работа с отдельными колонками
b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$score * 2

# создание новой колонки на основе существующей
mydata$ten_point_scale <- mydata$score * 2

summary(mydata$ten_point_scale)

# кол-во строк и столбцов в датафрейме
nrow(mydata)
ncol(mydata)

mydata$new_varible <- 0
mydata$number <- 1:nrow(mydata)
summary(mydata$number)


# выделение подмножеств дата-фрейма - обращение к строкам, стобцам и ячейкам

mydata$score[1:10]

mydata[2,3]
mydata[c(2,5,7),1]
mydata[101:111, 1]

mydata[5,]
mydata[,1] 

mydata[, 2:5]
head(mydata[,2:5])

##


# выборка подмножеств с условиями

mydata$gender
mydata$gender == 'female'
# выбрать объекты женского пола и столбцы с 1 по 3й
head(mydata[ mydata$gender == 'female', 1:3 ])

# выделение подмножества на основе условий с помощью ф-ции subset
head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))


# объединение нескольких датафреймов по строкам и столбцам
mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata6, mydata5)

