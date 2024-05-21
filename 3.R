library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")

all_data<-read.csv("C:\\Users\\Sumu\\Documents\\r16i_os26c.csv") 

head(all_data)

data = na.omit(select(all_data, status, l_marst, l_occup, l_diplom, lh5, l_age, lj6.2, lj10, lj35.1))

value = 99999996

data = subset(data, status<value & l_marst<value & l_occup<value & lh5<value & 
                l_age<value & lj6.2<value & lj10<value & lj35.1<value)

data["wed1"] = 0
data$wed1[which(data$l_marst == 6 | data$l_marst==2)] <- 1

data["wed2"] = 0
data$wed2[which(data$l_marst == 5 | data$l_marst==4)] <- 1

data["wed3"] = 0
data$wed3[which(data$l_marst == 1)] <- 1

data["sex"] = (data$lh5)%%2

data["higher_educ"] = 0
data$higher_educ[which(data$l_diplom == 6)] <- 1

data["city_status"] = 0
data$city_status[which(data$status == 1 | data$status==2)] <- 1

data["salary"] = (data$lj10 - mean(data$lj10))/sqrt(var(data$lj10))
data["age"] = (data$l_age - mean(data$l_age))/sqrt(var(data$l_age))
data["hours"] = (data$lj6.2 - mean(data$lj6.2))/sqrt(var(data$lj6.2))
data["field"] = (data$lj35.1 - mean(data$lj35.1))/sqrt(var(data$lj35.1))


data = select(data, wed1, wed2, wed3, sex, higher_educ, city_status, salary, age, hours, field)

data
model = lm(salary ~ wed1 + wed2 + wed3 + sex + higher_educ + city_status + age + hours + field, data = data)
summary(model)
vif(model)
# adj R^2 = 0.1468
# wed1        wed2        wed3        sex         higher_educ  city_status  age         hours       field 
# 3.272110    3.013500    2.169161    1.129335    1.081819     1.067926     1.260780    1.047480    1.063418
# Регрессоры линейно независимы друг от друга

model1 = lm(salary ~ wed1 + wed2  + sex + higher_educ + city_status + age + hours + field, data = data)
summary(model1)
vif(model1)
# adj R^2 = 0.1502
# wed1        wed2        sex         higher_educ city_status age         hours       field 
# 1.753942    1.957713    1.121169    1.078737    1.051514    1.233013    1.047373    1.061900

model2 = lm(salary ~ wed1  + sex + higher_educ + city_status + age + hours + field, data = data)
summary(model2)
vif(model2)
# adj R^2 = 0.1538
# wed1        sex         higher_educ city_status age         hours       field 
# 1.060421    1.108194    1.073359    1.040147    1.043056    1.043416    1.052810

model3 = lm(salary ~ wed1  + sex + higher_educ + city_status + age + hours, data = data)
summary(model3)
vif(model3)
# adj R^2 = 0.1568 
# wed1        sex         higher_educ city_status age         hours 
# 1.060109    1.086092    1.051695    1.034525    1.037047    1.041472

model4 = lm(salary ~ sex + higher_educ + city_status + age + hours, data = data)
summary(model4)
vif(model4)
# adj R^2 = 0.1568 
# sex         higher_educ city_status age         hours 
# 1.031923    1.050601    1.034120    1.036940    1.039384 

model5 = lm(salary ~ sex + higher_educ + city_status + hours, data = data)
summary(model5)
vif(model5)
# adj R^2 = 0.1564
# sex higher_educ city_status       hours 
# 1.026252    1.034186    1.023996    1.039328

model6 = lm(salary ~ sex + higher_educ  + hours, data = data)
summary(model6)
vif(model6)
# adj R^2 = 0.1542
# sex         higher_educ hours 
# 1.025282    1.014625    1.037859


#------------------------------------------------------------------------------#

# Рассмотрим модель, добавив hours*field

difmodel1 = lm(salary ~ sex + higher_educ + hours + I(hours*field), data = data)
summary(difmodel1)
vif(difmodel1)
# Переменные линейно независимые
# adj R^2 = 0.1534
# adj R^2 не изменился, а значимость у переменных уменьшилась
# Нет смысла добавлять hours*field

# Рассмотрим модель, добавив hours * age

difmodel2 = lm(salary ~ sex + higher_educ + hours + I(hours * age), data = data)
summary(difmodel2)
vif(difmodel2)
# Переменные линейно независимые
# adj R^2 = 0.1635
# adj R^2 незначительно изменился, и его значимость не велика
# Нет смысла добавить hours * age

difmodel3 = lm(salary ~ sex + higher_educ + hours + I(field*age), data = data)
summary(difmodel3)
vif(difmodel3)
# Переменные линейно независимые
# adj R^2 = 0.1591
# adj R^2 значительно не изменился
# Нет смысла добавлять field*age


#------------------------------------------------------------------------------#

# Рассмотрим модель, добавив log(abs(hours))

logmodel1 = lm(salary ~ sex + higher_educ + log(abs(hours)) + hours, data = data)
summary(logmodel1)
vif(logmodel1)
# Переменные линейно независимые
# adj R^2 = 0.1589
# adj R^2 значительно не изменился
# Нет смысла добавлять log(abs(hours))

# Рассмотрим модель, добавив log(abs(field))

logmodel2 = lm(salary ~ sex + higher_educ + log(abs(field)) + hours, data = data)
summary(logmodel2)
vif(logmodel2)
# Переменные линейно независимые
# adj R^2 = 0.1671
# adj R^2 значительно не изменился
# Нет смысла добавлять log(abs(field))


# Рассмотрим модель, добавив log(abs(field))

logmodel3 = lm(salary ~ sex + higher_educ + log(abs(age)) + hours, data = data)
summary(logmodel3)
vif(logmodel3)
# Переменные линейно независимые
# adj R^2 = 0.1608
# adj R^2 значительно не изменился, log(abs(age)) не значим
# Нет смысла добавлять log(abs(age))

#------------------------------------------------------------------------------#

powers <- seq(0.1,20,0.1)

length(powers)

adjR <- seq(1,200)
linearity <- seq(1,200)

for(i in 1:200){
  testmodel = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^powers[i]), data = data)
  adjR[i] = summary(testmodel)$adj.r.squared
  linearity[i] = max(vif(testmodel))
}

res = matrix(
  c(powers,adjR,linearity),
  ncol = 3,
  nrow = 200
)

colnames(res) <- c("Power","adjR^2","MaxVif")
rownames(res) <- c(powers)

res
max(res[201:400]) 
# i = 6.5 и линейно независимые переменные, adj R^2 = 0.2341593
# Можно добавить в модель abs(hours)^6.5

for(i in 1:200){
  testmodel = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^6.5) + I(abs(age)^powers[i]), data = data)
  adjR[i] = summary(testmodel)$adj.r.squared
  linearity[i] = max(vif(testmodel))
}

res = matrix(
  c(powers,adjR,linearity),
  ncol = 3,
  nrow = 200
)

res
max(res[201:400]) 
# adj R^2 = 0.2395734, незначительно повышается, смысла добавлять регрессор нет

for(i in 1:200){
  testmodel = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^6.5) + I(abs(field)^powers[i]), data = data)
  adjR[i] = summary(testmodel)$adj.r.squared
  linearity[i] = max(vif(testmodel))
}

res = matrix(
  c(powers,adjR,linearity),
  ncol = 3,
  nrow = 200
)

res
max(res[201:400]) 
# adj R^2 = 0.2352844, незначительно повышается, смысла добавлять регрессор нет

# Итого наилучшая модель:

model = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^6.5), data = data)
summary(model)
# все регрессоры значимы
# Положительная связь зарплаты от:
# наличия высшего образования
# среднего кол-ва рабочих часов в неделю, пола

# По этой модели наибольшую зарплату получают мужчины с высшем образованием,  
# c большим кол-вом рабочих часов в неделю

data1 = subset(data, wed1 == 0 & higher_educ == 1)
model = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^6.5), data = data1)
summary(model)
# adj R^2 = 0.037
# ни один регрессор не является значимым

# Наибольшую зарплату получают получают мужчины c большим кол-вом рабочих часов в неделю


data2 = subset(data, wed1 == 1 & wed2 == 0 & wed3 == 0 & city_status == 1)
model = lm(salary ~ sex + higher_educ  + hours + I(abs(hours)^6.5), data = data2)
summary(model)
# adj R^2 = 0.3487
# регрессоры значимы

# Наибольшую зарплату получают получают мужчины с высшем образованием,  
# c большим кол-вом рабочих часов в неделю
