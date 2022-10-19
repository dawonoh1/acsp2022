library('lme4')
library('lmerTest')
library('tidyverse')

setwd("C:\\Users\\ohdaw\\Dropbox\\GSD\\seoulmobility\\variable\\dataset")
dt <- read.csv("C:\\Users\\ohdaw\\Dropbox\\GSD\\seoulmobility\\variable\\dataset
               \\var_221012_12.csv", header = TRUE)
View(dt)

mydescriptive <- function(myvariable){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),3)
  mysd <- round(sd(myvariable),3)
  mymin <- round(min(myvariable),3)
  mymax <- round(max(myvariable),3)
  mydes <- matrix(c(mysize, mymean, mysd, mymin, mymax), ncol=5)
  colnames(mydes) <- c('n', 'mean', 'sd', 'min', 'max')
  mydes
}

mydescriptive(dt$average_time)

install.packages('data.table')
install.packages('mltools')
library('mltools')
library('data.table')
library("car")


dt$travel_type <- as.factor(dt$travel_type)
df<-one_hot(as.data.table(dt$travel_type))
data<-cbind(dt,df)

data$sex <- as.factor(data$sex)
da<-one_hot(as.data.table(data$sex))
data1<-cbind(data,da)

View(data1)
str(data1)

data1$ln_averagetime <- log(data1$average_time+1)
data1$ln_d_sum_pub   <- log(data1$d_sum_pub+1)
data1$ln_d_sum_ind   <- log(data1$d_sum_ind+1)
data1$ln_d_sum_edu   <- log(data1$d_sum_edu+1)
data1$ln_d_sum_com   <- log(data1$d_sum_com+1)
data1$ln_d_sum_res   <- log(data1$d_sum_res+1)
data1$ln_o_sum_pub   <- log(data1$o_sum_pub+1)
data1$ln_o_sum_ind   <- log(data1$o_sum_ind+1)
data1$ln_o_sum_edu   <- log(data1$o_sum_edu+1)
data1$ln_o_sum_com   <- log(data1$o_sum_com+1)
data1$ln_o_sum_res   <- log(data1$o_sum_res+1)
data1$ln_sum_transit <- log(data1$sum_transit+1)
data1$ln_sum_metro   <- log(data1$sum_metro+1)
data1$ln_sum_bus     <- log(data1$sum_bus+1)
data1$ln_ave_transit <- log(data1$ave_transit+1)
data1$ln_ave_metro   <- log(data1$ave_metro+1)
data1$ln_ave_bus     <- log(data1$ave_bus+1)
data1$agesq     <- (data1$age)*(data1$age)

m1<-lm(formula = ln_averagetime ~  V1_F + commute + days + agesq + genmz + V1_F:age
         + commute:genmz + o_per_pub + o_per_ind + o_per_edu + o_per_res + lum1_o
         + d_per_pub + d_per_ind + d_per_edu + d_per_com + lum1_d
         + ln_sum_transit, data = data1)
vif(m1)

m0<-lmer(formula = ln_averagetime ~  V1_F + commute + days + agesq + genmz + V1_F:age
       + commute:genmz + o_per_pub + o_per_ind + o_per_edu + o_per_res + lum1_o
       + d_per_pub + d_per_ind + d_per_edu + d_per_com + lum1_d
       + ln_sum_transit +(1|origin_code)+(1|destination_code), data = data1)

summary(m0)
write.table(m0, file = "crossclassified with dummy.txt", sep = ",", 
            quote = FALSE, row.names = F)

install.packages('sjPlot')
install.packages('sjmisc')
install.packages('sjlabelled')

library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(m0)
tab_model(m0, show.se = TRUE,  show.stat = TRUE)
