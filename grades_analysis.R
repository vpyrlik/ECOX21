#install.packages('gsheet')
library(gsheet)
library(ggplot2)

grades <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1U7ThHrCBM04LA3LbLbo3RMqvAVy60lpI6NTCvIi7KLY/edit?usp=sharing",1)

colnames(grades) <- c("Name", "Group", "email", "Quiz1")

ggplot(grades, aes(x=Group,y=Quiz1)) + 
  geom_boxplot()


