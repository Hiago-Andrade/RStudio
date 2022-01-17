
## Dados: Gasto mensal versus Renda mensal (dados usados nos slides da Aula 1) 
## Objetivo: Analise descritiva, grafico de dispersao, 
##           calcular coeficiente de correlacao, ajustar um modelo de 
##           regressao simples, interpretar as saidas do R.
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
#Grafico de dispersao:
plot(Renda, Gasto, pch=16, xlab = "Renda (R$)", ylab = "Gasto (R$)") #Grafico de dispersao


#--
#Calculo do coeficiente de correlacao linear amostral de Pearson:
cov(Renda, Gasto) #Covariancia
sd(Renda) #Desvio-padrao
sd(Gasto) #Desvio-padrao
cov(Renda, Gasto)/(sd(Renda)*sd(Gasto)) #Correlacao (manualmente)
cor(Renda, Gasto, method = c("pearson")) #Funcao do R para calcular a correlacao


#--
#Teste de significancia para a correlacao (requer normalidade):
qqnorm(Gasto); qqline(Gasto) #Se os ponstos não ficarem muito distantes da linha, mostra indicios de normalidade
qqnorm(Renda); qqline(Renda)

#Testes de normalidade:
#H_0:Os dados seguem uma distribuicao Normal
#H_1:Os dados nao seguem uma distribuicao Normal
shapiro.test(Gasto)
shapiro.test(Renda)

#Teste para a correlacao H_0:rho=0 versus H_1:rho!=0
cor.test(Gasto, Renda, alternative = c("two.sided"), method = c("pearson"))


#--
#Ajuste do modelo de regressao linear simples (MRLS):
cov(Renda, Gasto)/var(Renda) #Estimativa do MMQ para beta_1
mean(Gasto)-cov(Renda, Gasto)/var(Renda)*mean(Renda) #Estimativa do MMQ para beta_0


#O modelo ajustado foi: y=349.66+0.65*x

modelo <- lm(Gasto~Renda)   #Funcao do R para o ajuste do MRLS
summary(modelo)             #Mostra alguns resumos do ajuste
names(summary(modelo))
summary(modelo)$coefficients
summary(modelo)$sigma       #Estimativa do desvio-padrao do erro (sigma)
summary(modelo)$sigma^2     #Estimativa da variancia do erro (sigma^2)
summary(modelo)$r.squared*100   #Valor do R2 (Coeficiente de Determinacao)
cor(Gasto, Renda)^2*100          #Na RLS simples, R2=(r)^2

anova(modelo)               #Tabela ANOVA

#--
#Verificando a estimativa do MMQ para sigma^2:
var(modelo$residuals)*9/8 #Estimativa do MMQ para sigma^2


#--
#Grafico de dispersao com o modelo ajustado:
plot(Renda, Gasto, pch=16, xlab = "Renda (R$)", ylab = "Gasto (R$)") #Grafico de dispersao
abline(modelo, col="red", lwd=2)
coefs <- as.numeric(round(modelo$coefficients, 2))
text(x=2000, y=650, labels = parse(text=paste0('hat(y) == ', coefs[1], 
                                               ifelse(sign(coefs[2])==1, " + ", " - "), coefs[2],'*', '"Renda"')))

#--
#
print("Fim do script: Laboratorio 1, Parte A!")