#install.packages(c("geosphere",
#                   "foreach",
#                   "sandwich",
#                   "lmtest"))
library(geosphere)
library(foreach)
library(sandwich)
library(lmtest)
library(stargazer)
library(knitr)

#original data
setwd("~/ECOX21")
dta <- read.csv("NYC_2019.csv")

#sample data
set.seed(146)
smpl <- order(runif(nrow(dta)))[1:5000]
dta1 <- dta[smpl,]

#distance calc
#distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
withinXkm <- function(i,X) {
  sum(distm(x=c(dta1$longitude[i],dta1$latitude[i]),
            y=cbind(dta1$longitude[-i],dta1$latitude[-i]))[1,]<X*1000)
}
within1km <- sapply(1:nrow(dta1), function(i) withinXkm(i,1))
within5km <- sapply(1:nrow(dta1), function(i) withinXkm(i,5))

#new data
dta2 <- data.frame(price               = dta1$price,
                   room_type           = dta1$room_type,
                   minimum_nights      = dta1$minimum_nights,
                   reviews_per_month   = dta1$reviews_per_month,
                   neighbourhood_group = dta1$neighbourhood_group,
                   neighbourhood       = dta1$neighbourhood,
                   within1km           = within1km,
                   within5km           = within5km)
dta2 <- dta2[dta2$price>0,]
write.csv2(dta2,"NYC_2019_smpl5000_dists.csv")


#pairwise regressions
lm1 <- lm(log(price)~1+within5km, data=dta2)

plot(x=within5km, y=log(dta1$price),pch='.')
abline(lm1$coefficients, lwd=2, col='red')

cf1 <- coeftest(lm1, vcovHC, Inf, type="HC0")

lm2 <- lm(log(price)~1+within1km, data=dta2)
plot(x=within1km, y=log(dta1$price),pch='.')
abline(lm2$coefficients, lwd=2, col='red')
cf2 <- coeftest(lm2, vcovHC, Inf, type="HC0")

#multiple regressions
lm3 <- lm(log(price)~1+within1km+within5km, data=dta2)
cf3 <- coeftest(lm3, vcovHC, Inf, type="HC0")

lm4 <- lm(log(price)~1+within1km+within5km+neighbourhood_group, data=dta2)
cf4 <- coeftest(lm4, vcovHC, Inf, type="HC0")

lm5 <- lm(log(price)~1+.-neighbourhood, data=dta2)
cf5 <- coeftest(lm5, vcovHC, Inf, type="HC0")

stargazer(cf1,cf2,cf3,cf4,cf5,type='text')

rmarkdown::render("lec04.Rmd")
