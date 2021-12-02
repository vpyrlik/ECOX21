### libraries
#install.packages("glm")
#library(glm)
library(margins)
library(stargazer)

### data
setwd("~/ECOX21/stuff")
default <- read.csv('default.csv',sep=',',header=TRUE)
train_smpl <- 1:600
test_smpl  <- 601:700

### descriptive
summary(default[train_smpl,])

### GLMs
spec0 <- DEFAULT ~ .
spec1 <- DEFAULT ~ . + I(AGE^2)
spec2 <- DEFAULT ~ . + I(AGE^2) - INCOME + I(log(INCOME))
spec3 <- DEFAULT ~ . + I(AGE^2) - INCOME + I(log(INCOME)) - DTI

glm1 <- glm(spec0, data=default[train_smpl,], binomial(link = "probit"))
glm2 <- glm(spec0, data=default[train_smpl,], binomial(link = "logit"))

glm3 <- glm(spec1, data=default[train_smpl,], binomial(link = "probit"))
glm4 <- glm(spec1, data=default[train_smpl,], binomial(link = "logit"))

glm5 <- glm(spec2, data=default[train_smpl,], binomial(link = "probit"))
glm6 <- glm(spec2, data=default[train_smpl,], binomial(link = "logit"))

glm7 <- glm(spec3, data=default[train_smpl,], binomial(link = "probit"))
glm8 <- glm(spec3, data=default[train_smpl,], binomial(link = "logit"))

stargazer(glm1,glm3,glm5, glm7,
          glm2,glm4,glm6, glm8, type="text")

### CIs 
#.....

### predict
 treshold <- 0.45

 pred_prob1 <- predict(glm1,newdata = default[test_smpl,],type='response')
 pred_outcome1 <- as.numeric(pred_prob1 > treshold)
 
 table(default$DEFAULT[test_smpl], pred_outcome1)

 