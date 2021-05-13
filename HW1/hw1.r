setwd("C:/Statistics/HW1/") 
rm(list=ls())
library("mlbench")
library("ggplot2")
data("Ozone")
df = Ozone[,c("V1","V7")]
df = na.omit(df)
df$V1 = as.numeric(df$V1)
df$V7 = as.numeric(df$V7)

#Q1
#A
hist(df$V7,main = "Humidity levels at LAX",xlab= "Humidity percentage",breaks = 20)
#B
humidity_wo_lower = df[df$V7 > 19,]$V7
hist(humidity_wo_lower,main = "Humidity levels at LAX witout lower column",xlab= "Humidity percentage",breaks = 20)
#C
#c1
y_summer = df[df$V1>=7 & df$V1<=9 ,]$V7
hist(y_summer,main = "Humidity levels at LAX between july-september",xlab= "Humidity percentage")
#c2
sigma = 7
mu0 = 74
alpha = .05
n = length(y_summer)
y_mean = mean(y_summer)
y_sd = sd(y_summer)
tc_up = qt(1-alpha/2,n-1,lower.tail = T)
tc_low = qt(alpha/2,n-1,lower.tail = T)
t = (y_mean-mu0)/(y_sd/sqrt(n))
p = 2*pt(t,n-1,lower.tail = T)
ci_upper =  y_mean + tc_up*y_sd/sqrt(n)
ci_lowwer = y_mean + tc_low*y_sd/sqrt(n)
#Q2
df_table = read.csv("Q2.csv")
View(df_table)
femaleNumber = c(0,1,2,3,4)
df_table$Xi = as.numeric(df_table$Xi)
femaleFrequency = df_table[df_table$Xi<90,]$Xi
observed <- table(rep(femaleNumber,femaleFrequency))
View(observed)
barplot(observed,
        main = "Observed Females in births of 4 puppies",
        ylim = c(0,max(femaleFrequency)),
)

pv = pchisq(11.1852,df = 4,lower.tail = FALSE)
xc_2 = qchisq(0.95,df=4)

