#Датасет по швейцарским кантонам, 1888 год
library("ggplot2")
library("GGally")
library("dplyr")
library("psych")

dsswiss <- swiss
glimpse(dsswiss) #бросить ввзгляд на данные
describe(dsswiss) #описательные статистики
ggpairs(dsswiss) #куча скаттер-плотов

olsswiss <- lm(data = dsswiss,
               Fertility~Agriculture+Education+Catholic) #оцениваем модель методом МНК
coef(olsswiss) #смотрим бета-коэффициенты
fitted(olsswiss) #считает оценки таргетов по известным аргументам
residuals(olsswiss) #считает отклонения таргетов, т.е. разницу между фактическим таргетом и оценкой
deviance(olsswiss) #считает RSS

report <- summary(olsswiss) #выводит таблицу ANOVA
report$r.squared #выведет из репорта коэффициент детерминации

cor(dsswiss$Fertility, fitted(olsswiss)) #рассчитает выборочную корреляцию

justset <- data.frame(Agriculture = 0.5, Catholic = 0.5, Education = 20) #создаем новый датафрейм с теми же переменными, но новыми данными
predict(olsswiss, justset)
