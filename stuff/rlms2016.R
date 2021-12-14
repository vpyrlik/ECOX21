#Packages
library(foreign)
library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)
library(margins)

#Data
setwd("~/ECOX21/stuff")
r25 <- read.dta('r25h_os26c.dta', convert.factors = FALSE)
r25 <- r25 %>% filter(ug7   ==  1        &
                      uf14  <   99999990 &
                      uf14  >   0)

r25$kids <- rowSums(2016-r25[,grep("UB([1-9]|1[0-5])_5",colnames(r25))]<18,na.rm=TRUE)
r25$older <- rowSums(2016-r25[,grep("UB([1-9]|1[0-5])_5",colnames(r25))]>65,na.rm=TRUE)

dta<- data.frame(own_tv   = as.numeric(r25$uc9_5_2a==1),
                 n_all    = r25$u_nfm,
                 n_kids   = r25$kids,
                 n_old    = r25$older,
                 msk      = as.numeric(r25$region==138),
                 spb      = as.numeric(r25$region==141),
                 popul    = r25$popul/(10^6),
                 loginc   = log(r25$uf14),
                 livelong = as.numeric(r25$uf12_a == 1),
                 dacha    = as.numeric(r25$uc9_101a == 1),
                 tomatoes = as.numeric(r25$ud8_2a == 1))
dta$tomatoes[is.na(dta$tomatoes)] <- 0
rm(r25)

#Summary
stargazer(dta, type='text')

#Modeling
spec0 <- own_tv ~ .
spec1 <- own_tv ~ 1+n_kids+n_old+loginc+I(loginc^2)+dacha+tomatoes+livelong+msk+spb+I(log(popul)) 
spec2 <- own_tv ~ 1+n_kids+n_old+loginc+I(loginc^2)+dacha+msk+spb+I(log(popul)) 


glm0 <- glm(spec0, data=dta, family = binomial(link="probit"))
glm1 <- glm(spec1, data=dta, family = binomial(link="probit"))
glm2 <- glm(spec2, data=dta, family = binomial(link="probit"))

stargazer(glm0, glm1, glm2,
          type='text')




