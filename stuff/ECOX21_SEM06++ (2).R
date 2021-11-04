# Econometrics, HSE, Fall-2021
# seminar 6

library(tidyverse)
#library(broom)
library(sandwich)
library(lmtest)
#library(car)
library(stargazer)
library(margins) # Stata-like marginal effects


# Loading the data =============================================================
MEPS <- read_csv("MEPS.csv") # Medical expenditure panel survey


# Describing the data ==========================================================
MEPS %>% glimpse()
MEPS %>% summary()

MEPS %>%
  ggplot(aes(income, ltotexp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2))


# Regression ===================================================================
reg0 <- lm(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income, 
           data = MEPS)
coeftest(reg0, df = Inf, vcov. = vcovHC, type = "HC0")

reg1 <- lm(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income + I(income^2), 
           data = MEPS)
coeftest(reg1, df = Inf, vcov. = vcovHC, type = "HC0")




# Marginal effects =============================================================

## Preliminary calculations ----------------------------------------------------
Vb <- vcovHC(reg1, type = "HC0")
dim(Vb)

# Vbinc <- Vb[8:9,8:9]
x1 <- c("income","I(income^2)")
Vbinc <- Vb[x1,x1]

Vbinc1 <- Vb["income", "income"]
Vbinc2 <- Vb["I(income^2)", "I(income^2)"]
Covbinc1 <- Vb["income", "I(income^2)"]

b_inc  <- reg1$coefficients["income"]
b_inc2 <- reg1$coefficients["I(income^2)"]

other_avg <- sum(reg1$coefficients * 
                   c(1, mean(MEPS$suppins), mean(MEPS$phylim), mean(MEPS$actlim), 
                     mean(MEPS$totchr), mean(MEPS$age), mean(MEPS$female), 0, 0))

min(MEPS$income)
max(MEPS$income)
income_range <- seq(0, 300, by = 1)
alpha <- 0.05


## V. Pyrlik approach ----------------------------------------------------------
SEMEinc <- function(inc) {
  as.numeric(sqrt(t(c(1,2*inc))%*% Vbinc %*% c(1,2*inc))[1,1])
}

SEMEinc_vec <- sapply(income_range,SEMEinc)

CIlow <- b_inc + 2*b_inc2*income_range - qnorm((1-alpha/2))*SEMEinc_vec
CIhigh <- b_inc + 2*b_inc2*income_range + qnorm((1-alpha/2))*SEMEinc_vec

plot(x = income_range,
     y = b_inc + 2*b_inc2*income_range, type="l")
abline(0,0,col="gray")
lines(x=income_range, y=CIlow,col='red',lty=3)
lines(x=income_range, y=CIhigh,col='red',lty=3)

plot(x = MEPS$income, y=MEPS$ltotexp, pch='.')
lines(x = income_range,
      y = other_avg + b_inc*income_range + b_inc2*income_range^2, type="l")


## D. Tereshchenko approach ----------------------------------------------------
MEPS_effects <- tibble(income = income_range)

MEPS_effects <- MEPS_effects %>% mutate(MEinc = b_inc + 2*b_inc2*income,
                                        Vmeinc = Vbinc1 + 4*(income^2)*Vbinc2 + 4*income*Covbinc1,
                                        SEmeinc = sqrt(Vmeinc),
                                        CImelow = MEinc - qnorm((1-alpha/2))*SEmeinc,
                                        CImehigh = MEinc + qnorm((1-alpha/2))*SEmeinc)

MEPS_effects %>% ggplot(aes(x = income)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") + 
  geom_ribbon(aes(x = income, ymin = CImelow, ymax = CImehigh), fill = "grey70", alpha = 0.5) + 
  geom_line(aes(y = MEinc)) + 
  labs(x = "Income", y = "Marginal effect of income")
  

MEPS %>% 
  mutate(ltotexp_predict = other_avg + b_inc*income + b_inc2*income^2) %>%
  ggplot(aes(income, ltotexp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) +
  geom_line(aes(income, ltotexp_predict), col = "red")


## margins::margins ------------------------------------------------------------
reg1 %>% margins(variables = "income", 
                 at = list(income = c(0, 50, 100, 150, 200, 250, 300),
                           suppins = mean(MEPS$suppins),
                           phylim = mean(MEPS$phylim),
                           actlim = mean(MEPS$actlim),
                           totchr = mean(MEPS$totchr),
                           age = mean(MEPS$age), 
                           female = mean(MEPS$female)),
                 vcov = vcovHC(reg1, type = "HC0"), 
                 vce = "delta")

me1 <- reg1 %>% margins_summary(variables = "income", 
                                at = list(income = c(0, 50, 100, 150, 200, 250, 300)),
                                vcov = vcovHC(reg1, type = "HC0"), 
                                vce = "delta")

me2 <- reg1 %>% margins_summary(variables = "income", 
                                at = list(income = seq(from = 0, to = 300, by = 1)),
                                vcov = vcovHC(reg1, type = "HC0"), 
                                vce = "delta")

me1

me1 %>% 
  ggplot() +
  geom_point(aes(income, AME)) +
  geom_errorbar(aes(x = income, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

me2 %>% 
  ggplot() +
  geom_ribbon(aes(x = income, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) + 
  geom_line(aes(income, AME)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")









