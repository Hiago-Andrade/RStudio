
## Objetivo: Analise de residuos.
############################################################################

#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))


#--
#Definindo os dados:
Renda=c(1117.25,2486.12,1933.51,695.19,1280.50,1842.9,629.31,1137.82,757.85,695.05)
Gasto=c(1166.77,1951.0,1737.32,744.90,1544.75,1295.87,981.98,1050.07,645.03,615.69)


#--
#Analise descritiva (individual para cada variavel):
summary(Gasto) #Medidas de Posicao
sd(Gasto)      #Desvio-padrao
hist(Gasto)    #Histograma
boxplot(Gasto) #Box-plot

summary(Renda)
sd(Renda)
hist(Renda)
boxplot(Renda)


#--
#Analise do realcionamento entre as variaveis:
plot(Renda, Gasto, pch=16, xlab = "Renda (R$)", ylab = "Gasto (R$)") #Grafico de dispersao
cor(Renda, Gasto, method = c("pearson")) #Funcao do R para calcular a correlacao


#--
#Ajuste do modelo de regressao linear simples (MRLS):
modelo1 <- lm(Gasto ~ Renda)
modelo1  #Equa??????o do modelo ajustado: Gasto=349.659+0.655*Renda 
summary(modelo1)       #Mostra testes t e outros resumos do ajuste
names(summary(modelo1))
summary(modelo1)$coefficients #Estimatica dos coeficientes
summary(modelo1)$sigma       #Estimativa do desvio-padrao do erro (sigma)
summary(modelo1)$sigma^2     #Estimativa da variancia do erro (sigma^2)
summary(modelo1)$r.squared*100   #Valor do R2 (Coeficiente de Determinacao)

anova(modelo1)     #Mostra a tabela de Analise de Variancia (Teste F)

#--
# Fazer estimacao intervalar dos coeficientes:
confint(modelo1, level=0.95)  #Intervalos de Confianca para coeficientes

#--
# Fazer predicao dos valores m???dios da resposta a partir de um valor 
# da variavel explicativa: uso da reta de regressao
novo_x <- data.frame(Renda=1500)

#Intervalo de confianca para a media da resposta
predict(modelo1, newdata = novo_x, interval = "confidence")


#Intervalo de confianca para a valor individual da resposta
#O intervalo de confiança será maior
predict(modelo1, newdata=novo_x, interval = "prediction")


#Propriedade: a reta de MMQ sempre passa pelo ponto (mean(Renda), mean(Gasto))
mean(Renda) 
mean(Gasto)
predict(modelo1, newdata=data.frame(Renda=mean(Renda)), interval = "none")


#Predicao para vetor de 'novos' valores da variavel explicativa:
#O erro-padrao aumento conforme nos afastamos do ponto (mean(Renda), mean(Gasto))
novos_x <- data.frame(Renda = c(min(Renda), 900, mean(Renda), 1800, max(Renda)))
predict(modelo1, newdata=novos_x, interval = "none", se.fit = TRUE)


#Grafico com linhas dos intervalos de confianca e de predicao:
novos_x <- data.frame(Renda = seq(629, 2590, 0.5))

pred.plim <- predict(lm(Gasto~Renda), newdata=novos_x, interval = "prediction")
pred.clim <- predict(lm(Gasto~Renda), newdata=novos_x, interval = "confidence")
plot(Gasto,Renda, ylim=c(250,2750))
matplot(novos_x$Renda, cbind(pred.clim, pred.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", lwd = 2, ylab = "Valores Preditos", xlab="Renda",
        col=c("black", "red","red", "orange", "orange"), add=TRUE)

#
##--
# MODELO SEM INTERCEPTO: Incluir o "-1" no lado direito da formula.
modelo2 <- lm(Gasto ~ -1 +Renda)
summary(modelo2)
anova(modelo2)
confint(modelo2, level=0.95)  #Intervalos de Confianca para coeficientes



#
##--
###Analise de Residuos
residuos1 <- modelo1$residuals   #Calcula o vetor de residuos 
preditos1 <- predict(modelo1) #Calcula os valores preditos 

## Teste de Normalidade dos Erros
qqnorm(residuos1) ; qqline(residuos1)   #Grafico de probabilidade normal
shapiro.test(residuos1)   #Teste de normalidade de Shapiro Wilk

## Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(Renda, residuos1, main="Covari???vel (no modelo) vs. Res???duos") 
abline(h=0)


## Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(preditos1, residuos1, main="Ajustados vs. Res???duos")  #Transformacao logaritmica
abline(h=0)

if(!require(lmtest)){install.packages("lmtest");require(lmtest)}  #Instala e/ou carrega o pacote lmtest

#Breusch-Pagan teste (avalia homogeneidade da vari???ncia dos erros quando explicativa e' quantativa)
bptest(modelo1)

## Avaliando suposicao de independencia: grafico Residuos x Ordem de Coleta
plot(1:length(Gasto), residuos1, main="Ordem vs. Res???duos", xlab="Ordem") 
abline(h=0)

dwtest(modelo1, alternative = "two.sided") # Teste de Durbin-Watson (pacote lmtest)
dwtest(modelo1, alternative = "less") 
dwtest(modelo1, alternative = "greater")

#
##--
### Avaliando variavel externa (nao incluida na analise):
Sexo=c("M","M","M","F","M","F","M","F","F","F")
Sexo2=c(2,2,2,1,2,1,2,1,1,1)
plot(Renda,Gasto, pch=16, col=c("blue","blue","blue","red","blue","red","blue","red","red","red"))


plot(Sexo2, resid(modelo1), main="Covari???vel (fora do modelo) vs. Res???duos", 
     xlab="Sexo (1:Feminino, 2:Masculino)", lwd=2,
     col=c("blue","blue","blue","red","blue","red","blue","red","red","red"))  #Transformacao logaritmica
abline(h=0)

boxplot(residuos1~Sexo, col=c("red","blue"))  
boxplot(Gasto ~ Sexo, col=c("red","blue"))    


#--
#
print("Fim do script: Laboratorio 2, Parte A!")