
## Objetivo: Introduzir a pratica do ajuste de modelos de regressao 
##          linear multipla; fazer inferencia no modelo, avaliar qualidade 
##          ajuste, analise de res???duos e predicao.
############################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

##########################################################################
########################### EXEMPLO 1 ####################################
#--
## Ler conjunto de dados:
dados <- read.csv("dados_lucro_publicidade.csv", header=T, dec=",", sep=";")
str(dados)   #Examina a estrutura do data frame "dados"


#--
#Analise descritiva (individual para cada variavel):
summary(dados$Lucro.anual) #Medidas de Posicao
sd(dados$Lucro.anual)      #Desvio-padrao
hist(dados$Lucro.anual)    #Histograma
boxplot(dados$Lucro.anual) #Box-plot

summary(dados$capital) #Medidas de Posicao
sd(dados$capital)      #Desvio-padrao
hist(dados$capital)    #Histograma
boxplot(dados$capital) #Box-plot

summary(dados$publicidade) #Medidas de Posicao
sd(dados$publicidade)      #Desvio-padrao
hist(dados$publicidade)    #Histograma
boxplot(dados$publicidade) #Box-plot

#--
#Analise do relacionamento entre as variaveis:
with(dados, plot(capital, Lucro.anual, pch=16))
with(dados, cor(capital, Lucro.anual, method="pearson"))

with(dados, plot(publicidade, Lucro.anual, pch=16))
with(dados, cor(publicidade, Lucro.anual, method="pearson"))

pairs(dados, col="red", pch=16) #Compara??????es entre todas as vari???veis

cor(dados, method = "pearson") #matriz de correla??????es


if(!require(corrplot)){install.packages("corrplot"); require(corrplot)}  #Instala e/ou carrega o pacote corrplot
corrplot(cor(dados, method = "pearson"), method = 'number') #method = 'ellipse'


if(!require(car)){install.packages("car"); require(car)} # Instala pacote car
scatter3d(Lucro.anual ~ capital + publicidade, 
          surface=TRUE, point.col="blue", data = dados)


if(!require(rgl)){install.packages("rgl"); require(rgl)} # Instala pacote rgl
plot3d(x=dados$capital, y=dados$publicidade, z=dados$Lucro.anual,
       type = 's',  radius = 1, col='black')


#--
#Ajuste do modelo de regressao linear multipla (MRLS):
modelo1 <- lm(Lucro.anual ~ capital + publicidade, data = dados)
modelo1   #Equa??????o do modelo ajustado: Lucro=3,402-0,118*Capital+2,608*Publicidade 
summary(modelo1)       #Mostra testes t e outros resumos do ajuste
summary(modelo1)$coefficients #Estimatica dos coeficientes
summary(modelo1)$sigma       #Estimativa do desvio-padrao do erro (sigma)
summary(modelo1)$sigma^2     #Estimativa da variancia do erro (sigma^2)
summary(modelo1)$r.squared*100   #Valor do R2
summary(modelo1)$adj.r.squared*100   #Valor do R2 ajustado

#--
#Tabela ANOVA no modelo de regressao linear multipla (MRLS):
anova(modelo1)

modelo0 <- lm(Lucro.anual ~ 1, data = dados)
anova(modelo1, modelo0)     #Mostra a tabela de Analise de Variancia (Teste F)

if(!require(rms)){install.packages("rms"); require(rms)} # Instala pacote rms
modelo_ols <- ols(Lucro.anual ~ capital + publicidade, data = dados)
anova(modelo_ols)


#--
# Fazer estimacao intervalar dos coeficientes:
confint(modelo1, level=0.95)  #Intervalos de Confianca para coeficientes


##--
# Modelo sem a vari???vel capital:
modelo2 <- lm(Lucro.anual ~ publicidade, data = dados)
summary(modelo2)       #Mostra testes t e outros resumos do ajuste


##--
# Modelo sem a vari???vel capital:
modelo3 <- lm(Lucro.anual ~ -1 + publicidade, data = dados)
summary(modelo3)       #Mostra testes t e outros resumos do ajuste
confint(modelo3, level=0.95)  #Intervalos de Confianca para coeficientes




##########################################################################
########################### EXEMPLO 2 ####################################
#--
## Ler conjunto de dados:
delivery <- read.csv("dados_delivery.csv", header=T, dec=",", sep=";")
str(delivery)   #Examina a estrutura do data frame "dados"
delivery <- delivery[ ,-1] #Retirando primeira coluna

pairs(delivery) #Compara??????es entre todas as vari???veis

cor(delivery, method = "pearson") #matriz de correla??????es

if(!require(corrplot)){install.packages("corrplot"); require(corrplot)}  #Instala e/ou carrega o pacote corrplot
corrplot(cor(dados, method = "pearson")) #method = 'number', 'ellipse'


if(!require(car)){install.packages("car"); require(car)} # Instala pacote car
scatter3d(Tempo_Entrega ~ N_Itens + Distancia, 
          surface=TRUE, point.col="blue", data = delivery)


if(!require(rgl)){install.packages("rgl"); require(rgl)} # Instala pacote rgl
plot3d(x=delivery$Distancia, y=delivery$N_Itens, z=delivery$Tempo_Entrega,
       type = 's',  radius = 20, col='black')



saida1 <- lm(Tempo_Entrega ~ N_Itens + Distancia, data=delivery)
summary(saida1)

saida2 <- lm(Tempo_Entrega ~ -1 + N_Itens + Distancia, data=delivery)
summary(saida2)


#--
# Fazer predicao dos valores m???dios da resposta a partir de um valor 
# das variaveis explicativas: uso da reta de regressao
novo_x <- data.frame(N_Itens=13, Distancia=650)

#Intervalo de confianca para a media da resposta
predict(saida2, newdata = novo_x, interval = "confidence")


#Intervalo de confianca para a valor individual da resposta
predict(saida2, newdata=novo_x, interval = "prediction", level = 0.95)


#Predicao para vetor de 'novos' valores da variavel explicativa:
#O erro-padrao aumento conforme nos afastamos do ponto (mean(Renda), mean(Gasto))
novos_x <- data.frame(N_Itens = c(min(delivery$N_Itens), 5, mean(delivery$N_Itens), 21, max(delivery$N_Itens)), 
                      Distancia = c(min(delivery$Distancia), 150, mean(delivery$Distancia), 850, max(delivery$Distancia)))
predict(saida2, newdata=novos_x, interval = "confidence") #Predi??????o para a m???dia
predict(saida2, newdata=novos_x, interval = "prediction") #Predi??????o para valor individual

#
##--
###Analise de Residuos
residuos2 <- saida2$residuals   #Calcula o vetor de residuos 
preditos2 <- predict(saida2) #Calcula os valores preditos 

## Teste de Normalidade dos Erros
qqnorm(residuos2) ; qqline(residuos2)   #Grafico de probabilidade normal
shapiro.test(residuos2)   #Teste de normalidade de Shapiro Wilk
hist(residuos2)

## Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(preditos2, residuos2, main="Ajustados vs. Res???duos")  #Transformacao logaritmica
abline(h=0)

if(!require(lmtest)){install.packages("lmtest");require(lmtest)}  #Instala e/ou carrega o pacote lmtest
#Breusch-Pagan teste (avalia homogeneidade da vari???ncia dos erros quando explicativa e' quantativa)
bptest(saida2)

## Avaliando suposicao de independencia: grafico Residuos x Ordem de Coleta
plot(1:nrow(delivery), residuos2, main="Ordem vs. Res???duos", xlab="Ordem") 
abline(h=0)

dwtest(saida2, alternative = "two.sided") # Teste de Durbin-Watson (pacote lmtest)
dwtest(saida2, alternative = "less") 
dwtest(saida2, alternative = "greater")



#--
# Verificando transforma??????o Box-Cox:
if(!require(car)){install.packages("car");require(car)}  #Instala e/ou carrega o pacote car

with(delivery, boxCox(saida2, lambda = seq(-2, 2, 1/10))) #Estima o valor de lambda (resultado grafico)
boxcox <- with(delivery, boxCox(saida2, 
                                lambda = seq(-5, 5, by=0.01), plotit=FALSE)) #Resultados numericos do boxcox
lambda <- boxcox$x[which.max(boxcox$y)] #Valor otimo do 'lambda'
print(lambda)


#--
# Retirando algumas observa??????es:
with(delivery, plot(N_Itens, Tempo_Entrega))
with(delivery, identify(N_Itens, Tempo_Entrega))
with(delivery, plot(Distancia, Tempo_Entrega))
with(delivery, identify(Distancia, Tempo_Entrega))

saida3 <- lm(Tempo_Entrega ~ N_Itens + Distancia, data=delivery[-c(9,11,16,20,22),])
summary(saida3)

residuos3 <- saida3$residuals   #Calcula o vetor de residuos 
preditos3 <- predict(saida3) #Calcula os valores preditos 

qqnorm(residuos3) ; qqline(residuos3)   #Grafico de probabilidade normal
hist(residuos3)
shapiro.test(residuos3)   #Teste de normalidade de Shapiro Wilk

plot(preditos3, residuos3, main="Ajustados vs. Res???duos")  #Transformacao logaritmica
abline(h=0)

bptest(saida3)

plot(1:nrow(delivery[-c(9,11,16,20,22),]), residuos3, main="Ordem vs. Res???duos", xlab="Ordem") 
abline(h=0)

dwtest(saida3, alternative = "two.sided") # Teste de Durbin-Watson (pacote lmtest)


#--
#
print("Fim do script: Laboratorio 3, Parte A!")