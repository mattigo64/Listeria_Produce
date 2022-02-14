library(readxl)
Produce_data_for_risk_model_6_24_2019 <- read_excel("~/Dropbox/Matt Igo - Schaffner Lab/CPS Listeria Produce/Results/Produce data for risk model 6.24.2019.xlsx", 
                                                    sheet = "Overall")
Produce = Produce_data_for_risk_model_6_24_2019
Produce = Produce[-c(58,59),]
Inoc=as.factor(Produce$`Innoculation Methodology`)
Matr = as.factor(Produce$`inoculum matrix`)
Matr = relevel(Matr,'peptone water')
Rich = as.factor(Produce$`Inoculum matrix: Poor/Neutral/Rich`)
Measure = as.factor(Produce$`Inoculation measurement`)
Stor = as.factor(Produce$`Storage Containter`)
Stor = relevel(Stor, 'Petri dishes')
Perm = as.factor(Produce$Permeable)
Air = as.factor(Produce$Airflow)
Surf = as.factor(Produce$`Smooth or Rough`)
Fuz = as.factor(Produce$`Fuzzy (Y/N)`)
Ground = as.factor(Produce$`Below, At or Above Ground`)
Retail = as.factor(Produce$`Retail or Grower`)
RH = as.factor(Produce$`Relative Humidity (%)`)
mcurv = as.factor(Produce$mCurv)
ncurv = as.factor((Produce$nCurv))
rate = Produce$rate
temp = Produce$`Storage Temperature (°C)`
days = Produce$`Duration of Study (days)`
start1 = as.numeric(Produce$`Starting Inoculation Level`)
time0  = Produce$`Inoculation Population at time 0`
timeEnd = Produce$`Inoculation Population at final time`
change = Produce$`Total Population Change (log CFU/g)`
interval = Produce$tIntval
data = Produce$nData
min = Produce$yDatMin
max = Produce$yDatMax
y0 = Produce$y0
se = Produce$`se(fit)`
yEnd = Produce$yEnd
RR = Produce$`R^2_stat`
initial = Produce$initVal
maxVal= Produce$`Maxium population Reached`
timeMax = Produce$`Time of Max population (day)`
minPop = Produce$`Minimum population Reached`
minPopTime = Produce$`Time of Min population (days)`
levels(Stor)


groups = data.frame(cbind(temp,RH,Inoc,Matr,Rich, Measure, days,start1,time0,timeEnd,change,data,min,interval,max,mcurv,ncurv,rate,y0,yEnd,se,RR,initial,maxVal,timeMax,minPop,minPopTime,Stor,Perm,Air,Surf,Fuz,Ground,Retail))
stacks = stack(groups)
ano =  aov(values~ind, data = stacks)
summary(ano)
TukeyHSD(ano)

pro = data.frame(Produce)
######################
groups2 = data.frame(cbind(temp,RH,Inoc,Matr,Rich, Measure, days,start1,time0,timeEnd,change,data,min,max,mcurv,ncurv,rate,y0,se,RR,timeMax,minPopTime,Stor,Perm,Air,Surf,Fuz,Ground,Retail))
stacks2 = stack(groups2)
ano2 =  aov(values~ind, data = stacks2)
summary(ano2)
options(max.print = 10000)
TukeyHSD(ano2)
###############MaximumGrowthRate
library(car)
library(caret)

model1 = lm(rate~temp+Inoc+Perm+Surf+Fuz+time0+timeEnd+ Rich, data =Produce)
summary(model1)
library(car)
car::vif(model1)
aov1= aov(model1)
summary(aov1)
AIC(model1)
prediction=predict(model1,Produce)
RMSE(rate,prediction)

step(model1, k=2, direction = 'both')
sub_model1 = lm(formula = rate ~ temp + Perm  + time0 + timeEnd + Rich, data = Produce)
summary(sub_model1)
car::vif(sub_model1)
aov1 = aov(sub_model1)
summary(aov1)
library(olsrr)
ols_step_best_subset(sub_model1, Measure ='AIC')
as.data.frame(ols_step_all_possible(sub_model1))
AIC(sub_model1)
prediction_sub=predict(sub_model1,Produce)
RMSE(rate,prediction_sub)

Produce[length(Produce)]

sub_model2 = lm(formula = rate ~ temp*timeEnd*time0, data = Produce)
summary(sub_model2)
car::vif(sub_model2)
aov1 = aov(sub_model2)
summary(aov1)
AIC(sub_model2)
prediction_sub2=predict(sub_model2,Produce)
RMSE(rate,prediction_sub2)
step(sub_model2, k=2, direction = 'both')


single_model1=(lm(rate~time0,data=Produce))
summary(single_model1)
plot(rate~time0, data=Produce)
AIC(single_model1)
prediction_single1=predict(single_model1,Produce)
RMSE(rate,prediction_single1)

single_model2=(lm(rate~timeEnd,data=Produce))
summary(single_model2)
plot(rate~timeEnd, data=Produce)
AIC(single_model2)
prediction_single2=predict(single_model2,Produce)
RMSE(rate,prediction_single2)

single_model3=(lm(rate~temp,data=Produce))
summary(single_model3)
plot(rate~temp, data=Produce)
AIC(single_model3)
prediction_single3=predict(single_model3,Produce)
RMSE(rate,prediction_single3)

#################################
########################3
#############################
model2 = lm(rate~temp+RH+Inoc+ Matr+Rich+ Measure+ Stor+Perm+Air+Surf+Retail+Fuz+days+time0+timeEnd+change+data+ min+max+timeMax+minPopTime)
summary(model2)
aov2= aov(model2)
summary(aov2)

library(olsrr)
library(car)

step(model2,direction='both',k=2)
?step
###################
subset_model= lm(formula = rate ~ temp + RH + Inoc + time0 + timeEnd + data + 
                   timeMax + Rich, data = Produce)
summary(subset_model)
aov_subset = aov(subset_model)
summary(aov_subset)
ols_step_best_subset(subset_model)
ols_plot_cooksd_chart(subset_model)
Produce[which(cooks.distance(subset_model) > 0.5),]
###################
model3 = lm(rate~temp+RH+Ground+Inoc+ Matr+Rich+ Measure+ Stor+Perm+Air+Surf+Retail+Fuz+days+start1+time0+timeEnd+change+data+ min+max+y0+se+timeMax+minPopTime+start1+mcurv+ncurv)
summary(model3)
aov3= aov(model3)
summary(aov3)
step(model3, direction = 'both',k=2)

subset_model2 = lm(formula = rate ~ temp + start1 + time0 + timeEnd + min + timeMax + 
                     minPopTime)
summary(subset_model2)
aov_subset2 = aov(subset_model2)
summary(aov_subset2)
vif(subset_model2)
ols_step_best_subset(subset_model2)
##########
##########
#########


modelMax = lm(yDatMax~rate +temp+Ground+Inoc+RH+Measure+ Matr+Rich+ Stor+Perm+Air+Surf+Retail+Fuz+days+start1+time0+timeEnd+change+data+ yDatMin+y0+se+timeMax+minPopTime+start1+mcurv+ncurv, data = Produce)
summary(modelMax)
aovMax= aov(modelMax)
summary(aovMax)
step(modelMax,direction='both',k=2)

library(olsrr)
library(car)
library(tidyverse)
library(leaps)


subsetMax = lm(formula = max ~ RH + Stor + days + time0 + timeEnd + min + 
                 y0  + minPopTime + Surf, data = Produce)

summary(subsetMax)
aovSubMax = aov(subsetMax)
summary(aovSubMax)
ols_step_best_subset(subsetMax)

subsetMax1 = lm(formula = yDatMax ~ RH + Stor + days + time0 + timeEnd + yDatMin + 
                 y0+ minPopTime  + Surf, data = Produce)
summary(subsetMax)
aovSubMax1 = aov(subsetMax1)
summary(aovSubMax1)
ols_step_best_subset(subsetMax1)
vif(subsetMax1)
##########
subsetMax2 = lm(formula = timeEnd ~ Matr + days + time0 + timeEnd  + yDatMin + 
     y0  + minPopTime  + Fuz + Air, data = Produce)
summary(subsetMax2)
aovSubMax2 = aov(subsetMax2)
summary(aovSubMax2)
ols_step_best_subset(subsetMax2)


########
modelFin = lm(timeEnd~rate +temp+RH+Ground+Inoc+ Matr+Rich+ Measure+ Stor+Perm+Air+Surf+Retail+Fuz+days+start1+time0+change+data+ max+min+y0+se+timeMax+minPopTime+start1+mcurv+ncurv)
summary(modelFin)
aovFin= aov(modelFin)
summary(aovFin)
step(modelFin,direction='both',k=2)
#############
#####
############
subset_modelFin = lm(formula = timeEnd ~ temp + start1 + yDatMax  +  Stor+ change + rate
                   + yDatMin, data = Produce)
summary(subset_modelFin)
aov_subsetFin = aov(subset_modelFin)
summary(aov_subsetFin)
ols_step_best_subset(subset_modelFin)

##########
Produce = Produce[-c(58:59,16,70,107,51,52,89),]


plot(rate~temp,main = 'Growth Rate vs Temperature', ylab = 'Growth Rate', xlab = 'Temperature')
abline(lm(rate~temp), lwd=2,col='red')

plot(rate~time0,ylim=c(-0.5,5), main = 'Growth Rate vs Starting Concentration', ylab = 'Growth Rate', xlab = 'Starting Concentration')
abline(lm(rate~time0), lwd=2,col='red')

plot(rate~timeEnd,ylim=c(-0.5,5), main = 'Growth Rate vs Final Concentration', ylab = 'Growth Rate', xlab = 'Final Concentration')
abline(lm(rate~timeEnd), lwd=2,col='red')

plot(rate~start1,ylim=c(-0.5,5))
abline(lm(rate~start1), col='red',lwd=2)
summary(lm(rate~start1))

library(ggplot2)
library(lattice)
Produce$TT = ifelse(Produce$`Storage Temperature (°C)` <= 4, 0, 
                      ifelse((Produce$`Storage Temperature (°C)` >4) & (Produce$`Storage Temperature (°C)` <=10),1,
                             ifelse((Produce$`Storage Temperature (°C)` >10) & (Produce$`Storage Temperature (°C)` <=20),2,
                             ifelse(Produce$`Storage Temperature (°C)` >20,3,4))))

xyplot(rate~time0, groups =  TT, data = Produce,cex = 1, pch = c(0,1,2,3), col = 'black', main = '          Salmonella Rate of Reduction vs 
          Temperature with Serovar', ylab = 'Rate (log CFU/day/g)', ylim = c(-1,5))

