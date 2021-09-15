######################################
#                                    #
#      The Effect of Vitamin C       #
#   on Tooth Growth in Guinea Pigs   #
#                                    #
#         Владимир Пырлик            #
#           ЭКОНОМЕТРИКА             #
#                                    #
#             Неделя 3               #
#    Эмпирический пример по теме     #
#    "Парная линейная регрессия"     #
#                                    #
######################################

# На основе данных из работы
# Crampton, E. W. (1947). The growth of the odontoblast
# of the incisor teeth as a criterion of vitamin C intake
# of the guinea pig. The Journal of Nutrition, 33(5),
# 491--504. 10.1093/jn/33.5.491.

##################################
#             ПАКЕТЫ             #
##################################
# установка пакетов, запускать один раз или для обновления
#install.packages(c("datasets,
#                   sandwich,
#                   lmtest,
#                   stargazer"))

# подгружаем пакеты
library(datasets)
library(sandwich)
library(lmtest)
library(stargazer)

##################################
#             ДАННЫЕ             #
##################################
# загружаем данные
# краткое описание данных по ссылке (детали - в статье):
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
data("ToothGrowth")

# переменные для текущей работы
y <- ToothGrowth$len    #длина зубов
x <- ToothGrowth$dose   #доза витамина

# диаграмма рассеивания для наших данных
plot(x = x,y = y,
     main = "Диаграмма рассеивания",
     xlab = "Доза витамана C, мг",
     ylab = "Длина зубов, мм")

##################################
#           РЕГРЕССИЯ            #
##################################
# оценки МНК парной линейной регрессии
# без функции lm(), просто алгебра
b1 <- cov(x,y)/var(x)
b0 <- mean(y) - b1*mean(x)

# дорисовываем на график регрессионную линию
abline(b0,b1)

# оценка парной регрессии МНК, функция lm()
m1 <- lm(len~1+dose, data=ToothGrowth)

# оценка стандартных ошибок, робастная формула
cf1 <- coeftest(m1, df=Inf, vcov=vcovHC)

# вывод результатов в таблицу
stargazer(m1,cf1,type='text')

##################################
#      ПРОВЕРКА ГИПОТЕЗЫ         #
##################################
t1  <- cf1[2,1]/cf1[2,2]
Pv1 <- 1-pnorm(t1) #1-сторонняя!!!
        
##################################
#     АЛЬТЕРНАТИВНАЯ МОДЕЛЬ      #
##################################
# В этом пункте вкратце все предыдущие упражнения
# проделаны для альтернативной модели в log-log форме

plot(x = log(x),y = log(y),
     main = "Диаграмма рассеивания",
     xlab = "Доза витамана C, мм, log-шкала",
     ylab = "Длина зубов, мг, log-шкала")
b11 <- cov(log(x),log(y))/var(log(x))
b01 <- mean(log(y)) - b11*mean(log(x))
abline(b01,b11)

m2  <- lm(log(len)~1+log(dose), data=ToothGrowth)
cf2 <- coeftest(m2, df=Inf, vcov=vcovHC)

stargazer(m2, cf2,type='text')

t2  <- cf2[2,1]/cf2[2,2]
Pv2 <- 1-pnorm(t2)

