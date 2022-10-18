
require(reshape2)
install.packages("foreign")
install.packages("ggplot2")
install.packages("foreign")
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)
View(dat)

## fit ordered logit model and store results 'm'
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

## view a summary of the model
summary(m)


--------------------------------------------------------------------------



install.packages('lme4')
install.packages('lmer')
install.packages('lmerTest')
install.packages('tidyverse')
library('lme4')
library('lmerTest')
library('tidyverse')

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

setwd("C:\\Users\\ohdaw\\Dropbox\\GSD\\seoulmobility\\variable\\dataset")
dt <- read.csv("C:\\Users\\ohdaw\\Dropbox\\GSD\\seoulmobility\\variable\\dataset\\var_221012_11.csv", header = TRUE)
View(dt)

mydescriptive(dt$value)

install.packages('data.table')
install.packages('mltools')
library('mltools')
library('data.table')
library("car")


df<-one_hot(as.data.table(dt$travel_type))
dt$travel_type <- as.factor(dt$travel_type)
df<-one_hot(as.data.table(dt$travel_type))
data<-cbind(dt,df)
View(data)

da<-one_hot(as.data.table(data$sex))
data$sex <- as.factor(data$sex)
da<-one_hot(as.data.table(data$sex))
data1<-cbind(data,da)
View(data1)
str(data1)


m0<-lm(formula = average_time ~  o_per_pub + o_per_ind + o_per_res + o_per_edu +  d_per_ind + d_per_edu + d_per_com +  d_per_pub + ave_metro + ave_bus + V1_F:age, data = data1)
vif(m0)


m0<- lmer(value~ V1_min0130 + V1_min3160 + V1_min6190 + V1_min91120 + V1_min121150 + V1_min151180  + V1_min181210 + V1_min211240 + V1_min241over +
            d_per_pub + d_per_ind + d_per_etc + d_per_agr + d_per_edu + d_per_com + d_per_res +o_per_pub + o_per_ind + o_per_etc + o_per_agr + 
            o_per_edu + o_per_com + o_per_res + ave_metro + ave_bus+ (1|origin_code)+(1|destination_code), data = data)
summary(m0)
write.table(m0, file = "crossclassified with dummy.txt", sep = ",", quote = FALSE, row.names = F)


# load package
install.packages('sjPlot')
install.packages('sjmisc')
install.packages('sjlabelled')
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(m0)
tab_model(m0, show.se = TRUE,  show.stat = TRUE)


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

