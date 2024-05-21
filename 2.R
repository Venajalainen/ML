# ЧАСТЬ 1
#------------------------------------------------------------------------------#
library(car)

data = mtcars

model = lm( formula = mpg ~ hp + cyl + wt + drat, data = data)
summary(model)

# adj R^2 = 0.822
# 0.1<|t| для переменной hp
# 0.1<0.2<|t| для переменной cyl
# 0.001<|t|<0.01 для переменной wt
# 0.1<0.5<|t| для переменной drat

# Проверим переменные на линейную зависимость

testhp = lm( formula = hp ~ cyl + wt + drat, data = data)
summary(testhp)
# adj R^2 = 0.707

testcyl = lm( formula = cyl ~ hp + wt + drat, data = data)
summary(testcyl)
# adj R^2 = 0.82

testwt = lm( formula = wt ~ hp + cyl + drat, data = data)
summary(testwt)
# adj R^2 = 0.64

testdrat = lm( formula = drat ~ hp + cyl + wt , data = data)
summary(testdrat)
# adj R^2 = 0.58

vif(model)

# у переменной cyl adj R^2 > 0.75 и коэффициент вздутия дисперсии больше 5
# Следовательно переменная линейно зависима от других, значит её можно
# исключить из модели

new_model1 = lm( formula = mpg ~ hp  + wt + drat , data = data)
summary(new_model1)

# adj R^2 = 0.8194
# 0.001<|t|<0.01 для переменной hp
# отрицательная взаимосвязь от hp
# 0<|t|<0.001 для переменной wt
# отрицательная взаимосвязь от wt
# 0.1<|t| для переменной drat
# взаимосвязи нет
# Модель хорошая

#Т.к. p-value у переменной drat значителен, попробуем рассмотреть модель без него

new_model2 = lm( formula = mpg ~ hp  + wt , data = data)
summary(new_model2)

# adj R^2 = 0.8148
# 0.001<|t|<0.01 для переменной hp
# отрицательная взаимосвязь от hp
# 0<|t|<0.001 для переменной wt
# отрицатеьная взаимосвязь от wt
# Модель хорошая

# Т.к. adj R^2 незначительно уменьшился, то можно исключить переменную drat из
# модели

#------------------------------------------------------------------------------#

# Попробуем ввести в модель логарифмы регрессоров

testmodel = lm(formula = mpg ~ log(hp) + hp + wt, data = data)
vif(testmodel)
# log(hp)       hp       wt 
# 19.64903 16.92548  2.13662 
# Log(hp) зависит от hp

# Рассмотрим модель поменяв hp на log(hp)

logmodel = lm(formula = mpg ~ wt + log(hp), data = data)
summary(logmodel)
vif(logmodel)
# adj R^2 = 0.8494
# wt      log(hp) 
# 2.0509  2.0509

# Лучше заменить hp на log(hp), т.к. повышается adj R^2

testmodel = lm(formula = mpg ~ log(wt) + log(hp) + wt, data = data)
vif(testmodel)
# log(wt)    log(hp)  wt 
# 25.022002  2.150642 23.861558
# Log(wt) зависит от wt

# Рассмотрим модель поменяв wt на log(wt)

logmodel2 = lm(formula = mpg ~ log(hp) + log(wt), data = data)
summary(logmodel2)
vif(logmodel2)

# adj R^2 = 0.8804
# log(hp) log(wt) 
# 2.15064 2.15064 

# Лучше заменить wt на log(wt), т.к. повышается adj R^2

testmodel = lm(formula = mpg ~ log(cyl) + log(hp) + log(wt), data = data)
vif(testmodel)
# log(cyl)  log(hp)  log(wt) 
# 5.612032  4.268482 2.856366
# Log(cyl) зависит от log(wt)

# Добавим к модели log(cyl)

logmodel3 = lm(formula = mpg ~ log(hp) + log(cyl) + log(wt), data = data)
summary(logmodel3)
vif(logmodel3)
# adj R^2 = 0.8776
# log(hp)  log(cyl)  log(wt) 
# 4.268482 5.612032  2.856366

# Нет смысла добавлять log(cyl) т.к. есть связь между переменными и adj R^2
# уменьшился

# Добавим к модели log(drat)

logmodel4 = lm(formula = mpg ~ log(hp) + log(wt) + log(drat), data = data)
summary(logmodel4)
vif(logmodel4) 
# adj R^2 = 0.8768
# log(hp)   log(wt) log(drat) 
# 2.153493  3.163179  2.094015

# Нет смысла добавлять log(drat), т.к. adj R^2 уменьшился

# Добавим к модели log(drat)

difmodel1 = lm(formula = mpg ~ I(wt*hp) + log(wt) + log(hp), data = data)
summary(difmodel1)
vif(difmodel1)
# adj R^2 = 0.8857
# I(wt * hp)  log(wt)    log(hp) 
# 9.412075    3.298363   6.244111
# Нет смысла добавлять wt*hp, т.к. adj R^2 незначительно повысился и существует
# сильная связь между регрессорами

# Добавим к модели wt*cyl

difmodel2 = lm(formula = mpg ~ I(wt*cyl) + log(hp) + log(wt), data = data)
summary(difmodel2)
vif(difmodel2)
# I(wt * cyl)  log(hp)     log(wt) 
# 10.684974    3.267132    7.233995 
# adj R^2 = 0.8134
# Нет смысла добавлять wt*cyl, т.к. adj R^2 сильно уменьшился и существует силь
# ная связь между регрессорами

# Добавим к модели wt*drat

difmodel3 = lm(formula = mpg ~ log(hp) + I(wt*drat) + log(wt), data = data)
summary(difmodel3)
vif(difmodel3)
# log(hp)      I(wt * drat)  log(wt) 
# 2.152087     4.578266      5.623752
# adj R^2 = 0.879
# Нет смысла добавлять wt*drat, т.к. adj R^2 никак не изменился и существует
# связь между регрессорами

# Добавим к модели cyl*drat

difmodel4 = lm(formula = mpg ~ I(cyl*drat)+ log(wt) + log(hp), data = data)
summary(difmodel4)
vif(difmodel4)
# I(cyl * drat)  log(wt)       log(hp) 
# 3.173879       2.153259      4.435051
# adj R^2 = 0.8764
# Нет смысла добавлять переменную cyl*drat, т.к. adj R^2 уменьшился

# Добавим к модели cyl*hp

difmodel5 = lm(formula = mpg ~ I(cyl*hp) + log(hp) + log(wt), data = data)
summary(difmodel5)
vif(difmodel5)
# adj R^2 = 0.8811
# Нет смысла добавлять переменную cyl*hp, т.к. adj R^2 никак не изменяется

# Добавим к модели drat*hp

difmodel6 = lm(formula = mpg ~ I(drat*hp) + log(wt) + log(hp), data = data)
summary(difmodel6)
vif(difmodel6)
# I(drat * hp)  log(wt)     log(hp) 
# 6.694934      2.902857    10.831641 
# adj R^2 = 0.8788
# Нет смысла добавлять drat*hp, т.к. adj R^2 уменьшился и существует сильная 
# связь между регрессорами

# Добавим к модели hp^2

sqrmodel1 = lm(formula = mpg ~ I(hp^2) + log(wt) + log(hp), data = data)
summary(sqrmodel1)
vif(sqrmodel1)
# I(hp^2)  log(wt)  log(hp)
# 5.237717 2.368811 7.793333
# adj R^2 = 0.8823
# Нет смысла добавлять hp^2, т.к. adj R^2 незначительно повысился и существует связь
# между регрессорами

# Добавим к модели wt^2

sqrmodel2 = lm(formula = mpg ~ I(wt^2) + log(hp) + log(wt), data = data)
summary(sqrmodel2)
vif(sqrmodel2)
# I(wt^2)  log(hp)  log(wt) 
# 6.632804 2.152927 7.949625 
# adj R^2 = 0.8805 
# Нет смысла добавлять wt^2, т.к. adj R^2 никак не изменился и существует связь
# между регрессорами

# Добавим к модели cyl^2

sqrmodel3 = lm(formula = mpg ~ I(cyl^2) + log(hp) + log(wt), data = data)
summary(sqrmodel3)
vif(sqrmodel3)
# I(cyl^2)  log(hp)  log(wt) 
# 5.744311  4.557061 2.742566
# adj R^2 =  0.8763
# Нет смысла добавлять cyl^2, т.к. adj R^2 уменьшился и существует связь между 
# регрессорами

# Добавим к модели drat^2

sqrmodel4 = lm(formula = mpg ~ log(hp) + log(wt) + I(drat^2), data = data)
summary(sqrmodel4)
vif(sqrmodel4)
# log(hp)   log(wt)   I(drat^2) 
# 2.166968  3.138293  2.189750
# adj R^2 = 0.8762 
# Нет смысла добавлять cyl^2, т.к. adj R^2 уменьшился

# Итого наилучшая модель logmodel2


#------------------------------------------------------------------------------#
# 2 Часть
#------------------------------------------------------------------------------#

model = logmodel2
summary(model)

# A data frame with 32 observations
# Мы используем три переменных
# Итого 29 степеней свободы

t = qt(0.975, df = 29)
t
# 2.04523

# [-7.55524; -2.83076] - доверительный интервал для log(hp)
# 0 не входит в интервал, поэтому можно отвергнуть гипотезу, что он нулевой
# [-14.9362; -7.863798] - доверительный интервал для log(wt)
# 0 не входит в интервал,поэтому можно отвергнуть гипотезу, что он нулевой

new.data = data.frame(hp = 212 , wt =3.6)

sqrt(var(data$mpg- mean(data$mpg)))

predict(new_model2, new.data, interval = "confidence")
# fit      lwr     upr
# 15.81074 14.7067 16.91479
# Доверительный интервал прогноза узкий
