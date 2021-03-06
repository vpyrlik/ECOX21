######################################
#                                    #
#      The Effect of Vitamin C       #
#   on Tooth Growth in Guinea Pigs   #
#                                    #
#         �������� ������            #
#           ������������             #
#                                    #
#             ������ 3               #
#    ������������ ������ �� ����     #
#    "������ �������� ���������"     #
#                                    #
######################################

# �� ������ ������ �� ������
# Crampton, E. W. (1947). The growth of the odontoblast
# of the incisor teeth as a criterion of vitamin C intake
# of the guinea pig. The Journal of Nutrition, 33(5),
# 491--504. 10.1093/jn/33.5.491.

##################################
#             ������             #
##################################
# ��������� �������, ��������� ���� ��� ��� ��� ����������
#install.packages(c("datasets,
#                   sandwich,
#                   lmtest,
#                   stargazer"))

# ���������� ������
library(datasets)
library(sandwich)
library(lmtest)
library(stargazer)

##################################
#             ������             #
##################################
# ��������� ������
# ������� �������� ������ �� ������ (������ - � ������):
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
data("ToothGrowth")

# ���������� ��� ������� ������
y <- ToothGrowth$len    #����� �����
x <- ToothGrowth$dose   #���� ��������

# ��������� ����������� ��� ����� ������
plot(x = x,y = y,
     main = "��������� �����������",
     xlab = "���� �������� C, ��",
     ylab = "����� �����, ��")

##################################
#           ���������            #
##################################
# ������ ��� ������ �������� ���������
# ��� ������� lm(), ������ �������
b1 <- cov(x,y)/var(x)
b0 <- mean(y) - b1*mean(x)

# ������������ �� ������ ������������� �����
abline(b0,b1)

# ������ ������ ��������� ���, ������� lm()
m1 <- lm(len~1+dose, data=ToothGrowth)

# ������ ����������� ������, ��������� �������
cf1 <- coeftest(m1, df=Inf, vcov=vcovHC)

# ����� ����������� � �������
stargazer(m1,cf1,type='text')

##################################
#      �������� ��������         #
##################################
t1  <- cf1[2,1]/cf1[2,2]
Pv1 <- 1-pnorm(t1) #1-���������!!!
        
##################################
#     �������������� ������      #
##################################
# � ���� ������ ������� ��� ���������� ����������
# ��������� ��� �������������� ������ � log-log �����

plot(x = log(x),y = log(y),
     main = "��������� �����������",
     xlab = "���� �������� C, ��, log-�����",
     ylab = "����� �����, ��, log-�����")
b11 <- cov(log(x),log(y))/var(log(x))
b01 <- mean(log(y)) - b11*mean(log(x))
abline(b01,b11)

m2  <- lm(log(len)~1+log(dose), data=ToothGrowth)
cf2 <- coeftest(m2, df=Inf, vcov=vcovHC)

stargazer(m2, cf2,type='text')

t2  <- cf2[2,1]/cf2[2,2]
Pv2 <- 1-pnorm(t2)

