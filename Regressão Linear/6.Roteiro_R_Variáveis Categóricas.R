
## Objetivo: Modelos com variaveis categoricas; 
##             intera??????o entre vari???veis.
############################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

##########################################################################
########################### EXEMPLO 1 ####################################

#--
## Ler conjunto de dados:
dados <- read.csv("dados_gasto_renda_sexo.csv", header=T, dec=",", sep=";")
str(dados)   #Examina a estrutura do data frame "dados"


#--
#Analise descritiva (individual para cada variavel):
summary(dados$Renda) #Medidas de Posicao
sd(dados$Renda)      #Desvio-padrao
hist(dados$Renda)    #Histograma
boxplot(dados$Renda) #Box-plot

summary(dados$Gasto) #Medidas de Posicao
sd(dados$Gasto)      #Desvio-padrao
hist(dados$Gasto)    #Histograma
boxplot(dados$Gasto) #Box-plot

table(dados$Sexo)
boxplot(dados$Gasto ~ dados$Sexo) #Box-plot

#--
#Analise do relacionamento entre as variaveis:
with(dados, plot(Renda, Gasto, pch=16))
with(dados, cor(Renda, Gasto, method="pearson"))

if(!require(ggplot2)){ install.packages("ggplot2")}; require(ggplot2)
ggplot(dados, aes(x = Renda, y = Gasto, color=Sexo)) +    #Plota grafico
  geom_point(size=2, shape=16) +                           #Muda padr???o dos pontos  
  scale_color_manual(values=c("violetred", "royalblue")) +  #Muda cores
  geom_smooth(method = "lm", se=FALSE)                     #Coloca linhas



#--
#Ajuste do modelo de regressao linear multipla (MRLS):
str(dados)
dados$Sexo <- factor(dados$Sexo, labels=c("F", "M"))
modelo1 <- lm(Gasto ~ factor(Sexo), data = dados)
modelo1            #Equa??????o do modelo ajustado
summary(modelo1)   #Mostra testes t e outros resumos do ajuste
anova(modelo1)     #Tabela ANOVA
confint(modelo1, level=0.95)  #Intervalos de Confianca para coeficientes


##--
# Modelo com ambas as vari???veis:
modelo2 <- lm(Gasto ~ factor(Sexo) + Renda, data = dados)
modelo2            #Equa??????o do modelo ajustado
summary(modelo2)   #Mostra testes t e outros resumos do ajuste


##--
# Modelo com intera??????o entre as vari???veis:
modelo3 <- lm(Gasto ~ factor(Sexo) + Renda + factor(Sexo):Renda, data = dados)
modelo3            #Equa??????o do modelo ajustado
summary(modelo3)   #Mostra testes t e outros resumos do ajuste




##########################################################################
########################### EXEMPLO 2 ####################################
#--
## Ler conjunto de dados:
gordura <- read.csv("dados_percentual_gordura.csv", header=T, dec=",", sep=";")
str(gordura)   #Examina a estrutura do data frame "dados"

pairs(data.frame(gordura$brozek, gordura$adipos_IMC, gordura$abdom))

if(!require(car)){install.packages("car"); require(car)} # Instala pacote car
scatter3d(brozek ~ adipos_IMC + abdom, 
          surface=TRUE, point.col="blue", data = gordura)


if(!require(rgl)){install.packages("rgl"); require(rgl)} # Instala pacote rgl
plot3d(x=gordura$adipos_IMC, y=gordura$abdom, z=gordura$brozek,
       type = 's',  radius = 1, col='black')



saida1 <- lm(brozek ~ adipos_IMC + abdom + adipos_IMC:abdom, data = gordura)
saida1            #Equa??????o do modelo ajustado
summary(saida1)   #Mostra testes t e outros resumos do ajuste
anova(saida1, lm(brozek ~ 1, data=gordura))     #Tabela ANOVA
confint(saida1, level=0.95)  #Intervalos de Confianca para coeficientes


##--
# Alguns gr???ficos para a interacao:
novos <- data.frame(adipos_IMC=rep(c(18,24,35), each=100),
                    abdom=rep(seq(65,150, length=100), 3))
preditos <- predict(saida1, newdata=novos)
dados_plot <- data.frame(preditos, novos)

if(!require(ggplot2)){ install.packages("ggplot2")}; require(ggplot2)
ggplot(dados_plot, aes(x = abdom, y = preditos, color=as.factor(adipos_IMC))) + #Plota grafico
  geom_line(size = 1.5) +                           #Muda padr???o dos pontos  
  theme(legend.position=c(.9,.2)) +
  guides(color=guide_legend(title="IMC")) +
  labs(y = "Percentual de Gordura Estimado", x = "Circunfer???ncia Abdominal",size=4)


if(!require(interactions)){ install.packages("interactions")}; require(interactions)
interact_plot(model=saida1, pred=abdom, modx = adipos_IMC, alpha = 3) +
  theme(legend.position=c(.8,.2)) 


if(!require(interplot)){ install.packages("interplot")}; require(interplot)
interplot(m = saida1, var1 = "abdom", var2 = "adipos_IMC") + 
  xlab("adipos_IMC") + ylab("Coeficiente estimado para 'abdom'")



#--
#
print("Fim do script: Laboratorio 3, Parte B!")