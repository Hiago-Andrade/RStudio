
## Objetivo: Analise de pontos influentes
############################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

############### ###########################################################
########################### EXEMPLO 1 ####################################
#--
## Ler conjunto de dados:
gesell <- read.csv("dados_gesell_idade_ao_falar.csv", header=T, dec=",", sep=";")
str(gesell)   #Examina a estrutura do data frame "dados"

with(gesell, plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
identify(gesell$Idade_ao_Falar, gesell$Score_de_Gesell)
cor(gesell$Idade_ao_Falar, gesell$Score_de_Gesell, method = "pearson")

boxplot(gesell$Idade_ao_Falar, xlab= "Idade_ao_Falar", horizontal=TRUE)
boxplot(gesell$Score_de_Gesell, xlab= "Score_de_Gesell", horizontal=TRUE)

#--
# Ajuste do modelo de regressao linear simples:
modelo1 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell)
summary(modelo1)       #Mostra testes t e outros resumos do ajuste
anova(modelo1)

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo1))
qqnorm(resid(modelo1))  ;  qqline(resid(modelo1))
shapiro.test(resid(modelo1))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo1), resid(modelo1))  
abline(h=0)
identify(predict(modelo1), resid(modelo1))

require(lmtest)
bptest(modelo1)

#--
# Analise de Observacoes Atipicas

r.stud <- rstudent(modelo1)  # Calculo dos residuos studentizados
Hii <- hatvalues(modelo1)           #Calculo dos alavancas
d.Cook <- cooks.distance(modelo1)  # Calculos das distancias de Cook
df.betas <- dfbetas(modelo1)
df.fits <- dffits(modelo1)


##Salvando medidas no data.frame
gesell <- data.frame(gesell, 
                     r.stud=round(r.stud, 3), 
                     Hii=round(Hii, 3), 
                     d.Cook=round(d.Cook, 3), 
                     df.beta0=round(df.betas[,1], 3), 
                     df.beta1=round(df.betas[,2], 3), 
                     df.fits=round(df.fits, 3))

## Examinando os valores
n <- nrow(gesell) #tamanho da amostra
p <- 2        #numero parametros do modelo (incluindo beta0)
a.res <- gesell$OBS[abs(r.stud) > qt(0.95,n-p)]   #Identificando observacoes com residuos discrepantes
a.res #outlier

boxplot(Hii)  #procurando por valores discrepantes do Hii

a.Hii <- gesell$OBS[Hii > 2*p/n]   #identificando observacoes com Hii discrepantes
a.Hii
gesell[a.Hii,] #ponto de alavanca


a.cook <- gesell$OBS[d.Cook > 1]   #Identificando observacoes com D Cook discrepantes
a.cook
round(d.Cook, 3)


a.df.beta0 <- gesell$OBS[abs(df.betas[,1]) > 2/sqrt(n)]   #Identificando observacoes com dfbetas discrepantes
a.df.beta0
a.df.beta1 <- gesell$OBS[abs(df.betas[,2]) > 2/sqrt(n)]   #Identificando observacoes com dfbetas discrepantes
a.df.beta1
print(df.betas)

a.df.fits <- gesell$OBS[abs(df.fits) > 2/sqrt(p/n)]   #Identificando observacoes com dfbetas discrepantes
a.df.fits
print(df.fits)

gesell[unique(c(a.res, a.Hii, a.cook, a.df.beta0, a.df.beta1, a.df.fits)), ]


##Alguns graficos das medidas de influencia:
if(!require(olsrr)){install.packages("olsrr"); require(olsrr)}
ols_plot_cooksd_bar(modelo1)
ols_plot_cooksd_chart(modelo1)
ols_plot_dfbetas(modelo1)
ols_plot_dffits(modelo1)

#--
## Reajustando o modelo sem a observacao 15: 
modelo2 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell[-c(15),])
summary(modelo2)       #Mostra testes t e outros resumos do ajuste

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo2))
qqnorm(resid(modelo2))  ;  qqline(resid(modelo2))
shapiro.test(resid(modelo2))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo2), resid(modelo2))  
abline(h=0)
identify(predict(modelo2), resid(modelo2))

bptest(modelo2)

#--
# Analise de Observacoes Atipicas

r.stud2 <- rstudent(modelo2)  # Calculo dos residuos studentizados
Hii2 <- hatvalues(modelo2)           #Calculo dos alavancas
d.Cook2 <- cooks.distance(modelo2)  # Calculos das distancias de Cook
df.betas2 <- dfbetas(modelo2)
df.fits2 <- dffits(modelo2)

## Examinando os valores
n <- nrow(gesell) - 1 #tamanho da amostra
p <- 2        #numero parametros do modelo (incluindo beta0)
a.res2 <- gesell$OBS[which(abs(r.stud2) > qt(0.95,n-p))]   #Identificando observacoes com residuos discrepantes
a.res2 #outlier

boxplot(Hii2)  #procurando por valores discrepantes do Hii

a.Hii2 <- gesell$OBS[which(Hii2 > 2*p/n)]   #identificando observacoes com Hii discrepantes
a.Hii2
print(Hii2)


a.cook2 <- gesell$OBS[which(d.Cook2 > 1)]   #Identificando observacoes com D Cook discrepantes
a.cook2
print(d.Cook2)


a.df.beta02 <- gesell$OBS[which(abs(df.betas2[,1]) > 2/sqrt(n))]   #Identificando observacoes com dfbetas discrepantes
a.df.beta02
a.df.beta12 <- gesell$OBS[which(abs(df.betas2[,2]) > 2/sqrt(n))]   #Identificando observacoes com dfbetas discrepantes
a.df.beta12
print(df.betas2)

a.df.fits2 <- gesell$OBS[which(abs(df.fits2) > 2/sqrt(p/n))]   #Identificando observacoes com dfbetas discrepantes
a.df.fits2
print(df.fits2)

summary(modelo1)$coefficients
summary(modelo2)$coefficients
#A retirada observacao 15 nao modificou substancialmente as estimativas

summary(modelo1)$sigma  #Comparando variancia dos ajustes
summary(modelo2)$sigma
#A retirada observacao 15 diminui muito o desvio-padrao do erro

#Decis???o: RETIRAR A OBSERVACAO 15


#--
## Reajustando o modelo sem a observacoes 15 e 14: 
modelo3 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell[-c(14, 15),])
summary(modelo3)       #Mostra testes t e outros resumos do ajuste

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo3))
qqnorm(resid(modelo3))  ;  qqline(resid(modelo3))
shapiro.test(resid(modelo3))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo3), resid(modelo3))  
abline(h=0)
identify(predict(modelo3), resid(modelo3))

bptest(modelo3)

#--
# Analise de Observacoes Atipicas

r.stud3 <- rstudent(modelo3)  # Calculo dos residuos studentizados
Hii3 <- hatvalues(modelo3)           #Calculo dos alavancas
d.Cook3 <- cooks.distance(modelo3)  # Calculos das distancias de Cook
df.betas3 <- dfbetas(modelo3)
df.fits3 <- dffits(modelo3)

## Examinando os valores
n <- nrow(gesell) - 2 #tamanho da amostra
p <- 2        #numero parametros do modelo (incluindo beta0)
a.res3 <- gesell$OBS[which(abs(r.stud3) > qt(0.95,n-p))]   #Identificando observacoes com residuos discrepantes
a.res3 #outlier

boxplot(Hii3)  #procurando por valores discrepantes do Hii

a.Hii3 <- gesell$OBS[which(Hii3 > 2*p/n)]   #identificando observacoes com Hii discrepantes
a.Hii3
print(Hii3)


a.cook3 <- gesell$OBS[which(d.Cook3 > 1)]   #Identificando observacoes com D Cook discrepantes
a.cook3
print(d.Cook3)


a.df.beta03 <- gesell$OBS[which(abs(df.betas3[,1]) > 2/sqrt(n))]   #Identificando observacoes com dfbetas discrepantes
a.df.beta03
a.df.beta13 <- gesell$OBS[which(abs(df.betas3[,2]) > 2/sqrt(n))]   #Identificando observacoes com dfbetas discrepantes
a.df.beta13
print(df.betas3)

a.df.fits3 <- gesell$OBS[which(abs(df.fits3) > 2/sqrt(p/n))]   #Identificando observacoes com dfbetas discrepantes
a.df.fits3
print(df.fits3)

summary(modelo2)$coefficients
summary(modelo3)$coefficients
#A retirada da observacao 14 nao modificou substancialmente as estimativas

summary(modelo2)$sigma  #Comparando variancia dos ajustes
summary(modelo3)$sigma
#A retirada observacao 14 aumentou o desvio-padrao do erro

#Decis???o: NAO RETIRAR A OBSERVACAO 14


#--
## Reajustando o modelo sem a observacoes 15 e 2: 
modelo4 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell[-c(2, 15),])
summary(modelo4)       #Mostra testes t e outros resumos do ajuste

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo4))
qqnorm(resid(modelo4))  ;  qqline(resid(modelo4))
shapiro.test(resid(modelo4))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo4), resid(modelo4))  
abline(h=0)
identify(predict(modelo4), resid(modelo4))

bptest(modelo4)

summary(modelo2)$coefficients
summary(modelo4)$coefficients
#A retirada da observacao 2 modificou um pouco as estimativas

summary(modelo2)$sigma  #Comparando variancia dos ajustes
summary(modelo4)$sigma
#A retirada observacao 2 diminuiu muito pouco  o desvio-padrao do erro

#Decis???o: NAO RETIRAR A OBSERVACAO 2


#--
## Reajustando o modelo sem a observacoes 15 e 10: 
modelo5 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell[-c(15, 10),])
summary(modelo5)       #Mostra testes t e outros resumos do ajuste

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo5))
qqnorm(resid(modelo5))  ;  qqline(resid(modelo5))
shapiro.test(resid(modelo5))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo5), resid(modelo5))  
abline(h=0)
identify(predict(modelo5), resid(modelo5))

bptest(modelo4)

summary(modelo2)$coefficients
summary(modelo5)$coefficients
#A retirada da observacao 10 modificou um pouco as estimativas

summary(modelo2)$sigma  #Comparando variancia dos ajustes
summary(modelo5)$sigma
#A retirada observacao 10 diminuiu muito pouco o desvio-padrao do erro

#Decis???o: NAO RETIRAR A OBSERVACAO 10



#--
## Reajustando o modelo sem a observacoes 2, 10, 14 e 15: 
modelo6 <- lm(Score_de_Gesell ~ Idade_ao_Falar, data = gesell[-c(2, 10, 14, 15),])
summary(modelo6)       #Mostra testes t e outros resumos do ajuste

#--
# Analise de Residuos
# Teste de Normalidade dos Erros
hist(resid(modelo6))
qqnorm(resid(modelo6))  ;  qqline(resid(modelo6))
shapiro.test(resid(modelo6))

# Avaliando suposicao de variancia constante: grafico Residuos x Preditos
plot(predict(modelo6), resid(modelo6))  
abline(h=0)
identify(predict(modelo6), resid(modelo6))

bptest(modelo6)

summary(modelo2)$coefficients
summary(modelo6)$coefficients
#A retirada das observacoes 2,10,14,15 modificou as estimativas

summary(modelo2)$sigma  #Comparando variancia dos ajustes
summary(modelo6)$sigma
#A retirada das observacoes 2,10,14,15 diminuiu o desvio-padrao do erro,
# mas piorou muito o ajuste alem de reduzir o tamanho da amostra

#Decis???o FINAL: RETIRAR APENAS A OBSERVACAO 15


#--
## Gr???ficos do ajuste com diferentes observacoes removidas: 
par(mfrow=c(2,3))
with(gesell, plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo1, lwd=2)
with(gesell[-c(15),], plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo2, col="blue", lwd=2)
with(gesell[-c(14,15),], plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo3, col="red", lwd=2)
with(gesell[-c(2,15),], plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo4, col="green", lwd=2)
with(gesell[-c(10,15),], plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo5, col="orange", lwd=2)
with(gesell[-c(2,10,14,15),], plot(Idade_ao_Falar, Score_de_Gesell, pch=16))
abline(modelo6, col="purple", lwd=2)

#--
#
print("Fim do script: Laboratorio 4, Parte C!")