######################
#PACOTES
install.packages("readr") 
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library("corrplot")
install.packages("caret")
library(caret)
########################

#Exercício - dataset do concreto

#a)Matriz de Correlação
M <- cor(concrete_data)
corrplot(M, method = 'number')

#b)
#DIAGRAMA DE DISPERSÃO

#c)
#DIAGRAMA DE DISPERSÃO
#cement
ggplot(data = concrete_data, aes(x = cement, y = strength)) + geom_point(size = 3) +
  xlab("Cimento") + ylab("Força") + theme_classic()

#superplastic
ggplot(data = concrete_data, aes(x = superplastic, y = strength)) + geom_point(size = 3) +
  xlab("superplastic") + ylab("strength") + theme_classic()

#age
ggplot(data = concrete_data, aes(x = age, y = strength)) + geom_point(size = 3) +
  xlab("age") + ylab("strength") + theme_classic()

#d)
#REGRESSÃO LINEAR

modelo_1 <- lm(formula = strength ~., data = concrete_data)
summary(modelo_1)

#e)

#Diagnóstico dos resíduos

#1° - Verifiação da média (testar se a média é zero)

t.test(modelo_1$residuals)

#Valor-p = 1. como é maior que 0,05, nao ha evidencias para rejeitar a
#hipotese de media 0

#f e g)
#TIRANDO AS VARIAVEIS NÃO SIGNIFICATIVAS
modelo_2 <- lm(formula = strength ~ cement + slag + ash + water + superplastic + age, data = concrete_data)
summary(modelo_2)

#Diagnóstico dos resíduos

#1° - Verifiação da média (testar se a média é zero)

t.test(modelo_2$residuals)

#Valor-p = 1. como é maior que 0,05, nao ha evidencias para rejeitar a
#hipotese de media 0

#h)
#DADOS DO NOVO MATERIAL PARA PREVISÃO
novo_material <- data.frame(slag = 74, ash = 54, cement = 280, age = 45, water = 182, superplastic = 6, coarseagg = 972, fineagg = 773)

# Previsão e exibição do resultado
predict(modelo_2, newdata = novo_material)

