#Variant 20

data = swiss

mean1 = mean(data$Agriculture)
# 50.66 - среднее значение в Agriculture

var1 = var(data$Agriculture)
# 515.8 - дисперсия в Agriculture

CKO1 = sqrt(var(data$Agriculture))
# 22.7 - СКО в Agriculture

plot((data$Agriculture-mean1)/CKO1)+abline(a = 0, b=0, col="red")+
  abline(a=1,b=0,col="red") + abline(a=-1,b=0,col="red")

# Значения в Agriculture сильно отстоят от среднего значения

model1 = lm(Catholic~Agriculture, data)

summary(model1)

# Catholic = 0.74*Agriculture + 3.83
# 0<|t|<0.001 для Agriculture,
# существует положительная взаимосвязь между Catholic и Agriculture
# Adj R^2 = 0.14 < 0.3
# Модель плохая

#------------------------------------------------------------------------------#

plot(data$Infant.Mortality)+abline(a = mean(data$Infant.Mortality), b=0, col="red")+
  abline(a=mean(data$Infant.Mortality)+sqrt(var(data$Infant.Mortality)),b=0,col="red")+
  abline(a=mean(data$Infant.Mortality)-sqrt(var(data$Infant.Mortality)),b=0,col="red")

mean2 = mean(data$Infant.Mortality)
# 19.94 - среднее значение в Infant_Mortality

var2 = var(data$Infant.Mortality)
# 8.5 - дисперсия в Infant_Mortality

CKO2 = sqrt(var(data$Infant.Mortality))
# 2.9 - СКО в Infant_Mortality

plot((data$Infant.Mortality-mean2)/CKO2)+abline(a = 0, b=0, col="red")+
  abline(a=1,b=0,col="red") + abline(a=-1,b=0,col="red")

# Значения в Infant_Mortality не сильно отстоят от среднего значения

model2 = lm(Catholic ~ Infant.Mortality, data)

summary(model2)

# Catholic = 2.5*Infant_Mortality - 8.97
# 0.1<|t| для Infant_Mortality, 
# не существует взаимосвязи между Catholic и Infant_Mortality
# Adj R^2 = 0.009 < 0.3
# Модель плохая, зависимости нет