######################################
#                                    #
#                                    #
#                                    #
#                                    #
#         Владимир Пырлик            #
#           ЭКОНОМЕТРИКА             #
#                                    #
#             Неделя 4               #
#    Эмпирический пример по теме     #
# "Множественная линейная регрессия" #
#                                    #
######################################

# На основе данных из учебника
# Applied Linear Regression Models (McGraw Hill/Irwin Series:
# Operations and Decision Sciences) 4th Edition

##################################
#             ПАКЕТЫ             #
##################################
# установка пакетов, запускать один раз или для обновления
#install.packages(c("datasets,
#                   lmtest,
#                   stargazer"))

# подгружаем пакеты
library(sandwich)
library(lmtest)
library(stargazer)

##################################
#             ДАННЫЕ             #
##################################
# загружаем данные
setwd("~/ECOX21")
dta <- read.table("hospitaldata.txt", header=TRUE)

##################################
#           РЕГРЕССИЯ            #
##################################

# матрицы с данными
n <- nrow(dta)
Y <- dta$InfctRsk
X <- cbind(intercept = rep(1,n),
           stay      = dta$Stay,
           age       = dta$Age,
           beds      = dta$Beds)

hatQxx  <- t(X)%*%X/n
hatQxy  <- t(X)%*%Y/n
hatQxx1 <- solve(hatQxx)

hatbeta <- hatQxx1 %*% hatQxy

hatU    <- as.vector(Y - X%*%hatbeta) 

hatV1   <- as.numeric(var(hatU))*hatQxx1

hatOmega <- diag(hatU^2)
hatQxuxu <- t(X)%*%hatOmega%*%X/n
hatV2    <- hatQxx1 %*% hatQxuxu %*% hatQxx1

SE1 <- sqrt(diag(hatV1)/n)
SE2 <- sqrt(diag(hatV2)/n)

cbind(hatbeta, SE1, SE2)

# оценка МНК, функция lm()
m1 <- lm(InfctRsk~1+Stay+Age+Beds, data=dta)

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

