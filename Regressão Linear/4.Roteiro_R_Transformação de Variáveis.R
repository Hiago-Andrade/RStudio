
## Objetivo: Utilizacao de tranformacoes de variaveis no R.
############################################################################

#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

############################ EXEMPLO 1 ####################################
##-- 
## Ler conjunto de dados:
dados <- read.csv("dados_salario_experiencia.csv", header=T, dec=",", sep=";")
str(dados)   #Examina a estrutura do data frame "dados"

##-- 
## Algumas medidas descritivas:
nrow(dados)
summary(dados)


##-- 
## Analise do relacionamento entre as variaveis:
with(dados, plot(Experiencia, Salario, pch=16)) #Grafico de dispersao
with(dados, cor(Experiencia, Salario, method="pearson")) #Coef. de correlacao lienar
text(x=25, y=2, labels = "r=0,9704", font=2)


#--
# Ajuste do modelo de regressao linear simples (MRLS): funcao lm()
modelo1 <- lm(Salario ~ Experiencia, data=dados)
modelo1
summary(modelo1)   #Mostra testes t e outros resumos
anova(modelo1)     #Mostra a tabela de Analise de Variancia
confint(modelo1, level=0.95)  #Intervalos de Confianca para os coeficientes
abline(modelo1, col="red", lwd=2)    #Plota a reta ajustada

#--
# Extraindo o vetor de residuos:
residuos1 <- modelo1$residuals   #Calcula o vetor de residuos 
hist(residuos1)

#--
# Teste de normalidade dos residuos:
qqnorm(residuos1) ; qqline(residuos1)   #Grafico de probabilidade normal
shapiro.test(residuos1)   #Teste de normalidade de Shapiro Wilk

#--
# Verificacao da homocedasticidade dos residuos:
preditos1 <- predict(modelo1)
plot(preditos1, residuos1)
abline(h=0)

if(!require(lmtest)){install.packages("lmtest");require(lmtest)}  #Instala e/ou carrega o pacote lmtest
#Breusch-Pagan teste (avalia homogeneidade da variancia dos erros quando explicativa e' quantativa)
with(dados, bptest(modelo1))


#--
# Aplicando transformacao de Box-Cox na variavel resposta:
if(!require(car)){install.packages("car");require(car)}  #Instala e/ou carrega o pacote car

with(dados, boxCox(Salario ~ Experiencia)) #Estima o valor de lambda (resultado grafico)

boxcox1 <- with(dados, boxCox(Salario ~ Experiencia, 
                              lambda = seq(-5, 5, by=0.01), plotit=FALSE)) #Resultados numericos do boxcox
lambda <- boxcox1$x[which.max(boxcox1$y)] #Valor otimo do 'lambda'
print(lambda)
dados$Salario_boxcox <- (dados$Salario^lambda - 1)/lambda
View(dados)

modelo2 <- lm(Salario_boxcox ~ Experiencia, data=dados)
modelo2
summary(modelo2)   #Mostra testes t e outros resumos
anova(modelo2)     #Mostra a tabela de Analise de Variancia
confint(modelo2, level=0.95)  #Intervalos de Confianca para os coeficientes

residuos2 <- modelo2$residuals   #Calcula o vetor de residuos 
hist(residuos2)

qqnorm(residuos2); qqline(residuos2)   #Grafico de probabilidade normal
shapiro.test(residuos2)   #Teste de normalidade de Shapiro Wilk

preditos2 <- predict(modelo2)
plot(preditos2, residuos2)
abline(h=0)
with(dados, bptest(modelo2))



#--
# Aplicando transformacao raiz quadrada (lambda=0,5):
with(dados, plot(Experiencia, sqrt(Salario)))
modelo3 <- lm(sqrt(Salario) ~ Experiencia, data = dados)
summary(modelo3)
residuos3 <- modelo3$residuals
qqnorm(residuos3) ; qqline(residuos3)   #Grafico de probabilidade normal
shapiro.test(residuos3)   #Teste de normalidade de Shapiro Wilk
preditos3 <- predict(modelo3)
plot(preditos3, residuos3)
abline(h=0)
with(dados, bptest(modelo3))



#--
# Aplicando transformacao logaritmica (lambda=0):
with(dados, plot(Experiencia, log(Salario)))
modelo4 <- lm(log(Salario) ~ Experiencia, data = dados)
summary(modelo4)
residuos4 <- modelo4$residuals
qqnorm(residuos4) ; qqline(residuos4)   #Grafico de probabilidade normal
shapiro.test(residuos4)   #Teste de normalidade de Shapiro Wilk
preditos4 <- predict(modelo4)
plot(preditos4, residuos4)
abline(h=0)
with(dados, bptest(modelo4))


############################ EXEMPLO 2 ####################################
### Dados sobre velocidade de moinhos de vento (wind) e energia gerada (DC)

##-- 
## Carregando o conjunto de dados:
wind <- c(5.00, 6.00, 3.40, 2.70, 10.0, 9.70, 9.55, 3.05, 8.15, 6.20, 2.90, 6.35, 4.60,
          5.80, 7.40, 3.60, 7.85, 8.80, 7.00, 5.45, 9.10, 10.2, 4.10, 3.95, 2.45)

DC <- c(1.582, 1.822, 1.057, 0.500, 2.236, 2.386, 2.294, 0.558, 2.166, 1.866,
        0.653, 1.930, 1.562, 1.737, 2.088, 1.137, 2.179, 2.112, 1.800, 1.501,
        2.303, 2.310, 1.194, 1.144, 0.123)

length(DC)

##-- 
## Analise do relacionamento entre as variaveis:
plot(wind, DC, pch=16) #Grafico de dispersao
cor(wind, DC, method="pearson") #Coef. de correlacao lienar
text(x=9, y=0.15, labels = "r=0,9351", font=2)


#--
# Ajuste do modelo de regressao linear simples (MRLS): funcao lm()
ajuste1 <- lm(DC ~ wind)
abline(coefficients(ajuste1), col = 'red', lwd = 2)
summary(ajuste1)

res1 <- ajuste1$residuals
qqnorm(res1) ; qqline(res1)   #Grafico de probabilidade normal
shapiro.test(res1)   #Teste de normalidade de Shapiro Wilk

pred1 <- predict(ajuste1)
plot(pred1, res1)
abline(h=0)
bptest(ajuste1)

#--
# Possibilidades de transformacoes na vari???vel explicativa X:
plot(wind, DC)

wind2 <- wind^2
plot(wind2,DC)

wind_sqrt <- sqrt(wind)
plot(wind_sqrt,DC)

wind_log <- log(wind)
plot(wind_log,DC)

wind_inv <- 1/wind 
plot(wind_inv,DC)

#--
# Ajuste do modelo y = beta_0 + beta_1 (1/x) + epsilon:
ajuste2 <- lm(DC ~ wind_inv)
summary(ajuste2)

res2 <- ajuste2$residuals
qqnorm(res2) ; qqline(res2)   #Grafico de probabilidade normal
shapiro.test(res2)   #Teste de normalidade de Shapiro Wilk

pred2 <- predict(ajuste2)
plot(pred2, res2)
abline(h=0)
bptest(ajuste2)


#--
#
print("Fim do script: Laboratorio 2, Parte B!")