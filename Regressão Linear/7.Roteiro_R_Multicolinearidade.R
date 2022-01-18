
## Objetivo: Multicolinearidade.
############################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

############### ###########################################################
########################### EXEMPLO 1 ####################################
#--
## Ler conjunto de dados:
gordura <- read.csv("dados_percentual_gordura.csv", header=T, dec=",", sep=";")
str(gordura)   #Examina a estrutura do data frame "dados"
names(gordura) #Mostra nomes das vari???veis no banco de dados

variaveis <- c("neck", "chest", "abdom", "hip", "thigh", "knee",      
               "ankle", "biceps", "forearm", "wrist")

pairs(gordura[,variaveis])

round(cor(gordura[,variaveis], method = "pearson"), 3) #matriz de correla??????es

if(!require(corrplot)){install.packages("corrplot"); require(corrplot)}  #Instala e/ou carrega o pacote corrplot
corrplot(cor(gordura[,variaveis], method = "pearson"), method = 'number', type = 'lower')


if(!require(psych)){install.packages("psych"); require(psych)} 
pairs.panels(gordura[,variaveis], 
             smooth = FALSE, # show smoothed linesmethod = "pearson", # correlation method
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             cex.cor = 4
) 


#--
# Ajuste do modelo de regressao linear multipla (MRLS):
modelo1 <- lm(weight ~ neck + chest + abdom + hip + thigh + knee + 
                ankle + biceps + forearm + wrist, data = gordura)
summary(modelo1)       #Mostra testes t e outros resumos do ajuste


if(!require(car)){install.packages("car");require(car)}      
vif(modelo1) #Calculo do'Variance Inflation Factor' (VIF) atraves do pacote 'car'


#--
# Retirando a vari???vel de maior VIF ("hip"):
modelo2 <- lm(weight ~ neck + chest + abdom + thigh + knee + 
                ankle + biceps + forearm + wrist, data = gordura)

summary(modelo2)       #Mostra testes t e outros resumos do ajuste
vif(modelo2)



#--
# Retirando a vari???vel de maior VIF ("abdom"):
modelo3 <- lm(weight ~ neck + chest + thigh + knee + 
                ankle + biceps + forearm + wrist, data = gordura)

summary(modelo3)       #Mostra testes t e outros resumos do ajuste
vif(modelo3)


#--
# Retirando a vari???vel "forearm" (n???o significativa):
modelo4 <- lm(weight ~ neck + chest + thigh + knee + 
                ankle + biceps + wrist, data = gordura)

summary(modelo4)       #Mostra testes t e outros resumos do ajuste
vif(modelo4)

#--
# Retirando a vari???vel "biceps" (n???o significativa):
modelo5 <- lm(weight ~ neck + chest + thigh + knee + 
                ankle + wrist, data = gordura)

summary(modelo5)       #Mostra testes t e outros resumos do ajuste
vif(modelo5)

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

require(lmtest)
bptest(modelo5)


############### ###########################################################
########################### EXEMPLO 2 ####################################
#--
## Ler conjunto de dados:
pulse <- read.csv("dados_pulse_activity.csv", header=T, dec=",", sep=";")
str(pulse)   #Examina a estrutura do data frame "dados"
names(pulse) #Mostra nomes das vari???veis no banco de dados

saida1 <- lm(Pulse2.Pulse1 ~ Height + Weight + 
               factor(Smokes) + factor(Activity), data=pulse)
summary(saida1)
vif(saida1)

#--
#
print("Fim do script: Laboratorio 4, Parte A!")