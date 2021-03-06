#  Open with Encoding Windows 1251   # 

######################################
#                                    #
#  � ����� ������������� ���������   #
#      � ������������ ���������      #
#                                    #
#         �������� ������            #
#           ������������             #
#                                    #
#             ������ 4               #
#    ������������ ������ �� ����     #
# "������������� �������� ���������" #
#                                    #
######################################

# �� ������ ������ �� ��������
# Applied Linear Regression Models (McGraw Hill/Irwin Series:
# Operations and Decision Sciences) 4th Edition

##################################
#             ������             #
##################################
# ��������� �������, ��������� ���� ��� ��� ��� ����������
#install.packages(c("datasets,
#                   lmtest,
#                   stargazer"))

# ���������� ������
library(sandwich)
library(lmtest)
library(stargazer)

##################################
#             ������             #
##################################
# ��������� ������
setwd("~/ECOX21")
dta <- read.table("hospitaldata.txt",
                  header=TRUE)

##################################
#           ���������            #
##################################

## ������ "�������"
# ���������� ����������
n <- nrow(dta)

# ������� � �������
Y <- dta$InfctRsk
X <- cbind(intercept = rep(1,n),
           stay      = dta$Stay,
           age       = dta$Age,
           beds      = dta$Beds)

# ���������� ��� ���������� ������
hatQxx  <- t(X) %*% X / n 
hatQxy  <- t(X) %*% Y / n
hatQxx1 <- solve(hatQxx)

# ������ ���
hatbeta <- hatQxx1 %*% hatQxy

# ������ ��������
hatU    <- as.vector(Y - X%*%hatbeta) 

# ��-��������� ������
# ������� ���������� ������ 
hatV1   <- var(hatU)*hatQxx1 

# ��������� (����� �����) ������
# ������� ���������� ������
hatVxu <- t(X) %*% diag(hatU^2) %*% X/n
hatV2  <- hatQxx1 %*% hatVxu %*% hatQxx1

# ��� ������ ����������� ������
SE1 <- sqrt(diag(hatV1)/n) # ��-��������� SE
SE2 <- sqrt(diag(hatV2)/n) # ��������� SE (White's)

# "�������" � ������������
cbind(hatbeta, SE1, SE2)

## ������ ����������� ����������
# ������ ���, ������� lm()
m1 <- lm(InfctRsk ~ 1 + Stay + Age + Beds,
         data=dta)

# ������ ����������� ������,
# ��������� �������
cf1 <- coeftest(m1, df=Inf, vcov=vcovHC, type="HC0")

# ����� ����������� � �������
stargazer(m1,cf1,type='text')

##################################
#      �������� ��������         #
##################################
t1  <- cf1[4,1]/cf1[4,2]
Pv1 <- 1-pnorm(t1) #1-���������!!!
        
