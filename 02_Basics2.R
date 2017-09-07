library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)

data("whiteside")
attributes(whiteside)
tapply(whiteside$Temp, whiteside$Insul, median)
class(tapply(whiteside$Temp, whiteside$Insul, median))

aggregate(whiteside$Temp, list(whiteside$Insul), median)
class(aggregate(whiteside$Temp, list(whiteside$Insul), median))

library(car)
data("salaries")

Salaries %>% str()

rep(rnorm(1), 10)
replicate(10, rnorm(1))

#####
num_pred = read.csv("E:\\Coursework\\Spring 17\\R\\Session 3\\Session 3\\data\\NumericPred.csv")
bin_pred = read.csv("E:\\Coursework\\Spring 17\\R\\Session 3\\Session 3\\data\\BinaryPred.csv")

num_pred = num_pred %>% 
        rename(model1 = Predicted..Model1., model2 = Predicted..Model2.)

a = num_pred$Target
m = num_pred$model1

colnames(bin_pred)
k = 10

bin_pred %>% head()

fun_metrics = function(df = bin_pred, actual = bin_pred$Target, model = bin_pred$Model1){
        metrics = c(LL = 0, AIC = 0, BIC = 0, R2 = 0)
        metrics[1] = sum(ifelse(actual==1, log(model), log(1 - model)))
        metrics[2] = -2*metrics[1] + 2*k
        metrics[3] = -2*metrics[1] + 2*k*log(length(actual))
        metrics[4] = 1- sum((actual - model)^2)/sum((actual - mean(actual))^2)
        return(metrics)
}

m_compare = cbind(fun_metrics(),fun_metrics(df = bin_pred, model = bin_pred$Model2))
m_compare

cutoff = seq(0,1, 0.01)
op = vector()

for (i in cutoff) {
        MP = ifelse(bin_pred$Model2 >= i, 1, 0)
        TN = sum(MP[bin_pred$Target == 0] == 0)
        TP = sum(MP[bin_pred$Target == 1] == 1)
        FN = sum(MP[bin_pred$Target == 1] == 0)
        FP = sum(MP[bin_pred$Target == 0] == 1)
        TPR = TP/(TP + FN)
        FPR = FP/(TN + FP)
        vec = cbind(i, TP, FP, TPR, FPR)
        op = rbind(op, vec)
}

op = as.data.frame(op)
plot(op$FPR, op$TPR, type = "l", main = "ROC Curve")



