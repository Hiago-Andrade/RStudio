
## Objetivo: Analise descritiva, grafico de dispersao, 
##           calcular coeficiente de correlacao, ajustar um modelo de 
##           regressao simples, interpretar as saidas do R.
############################################################################

#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))

##-- 
## Ler conjunto de dados:
## O arquivo "dados_delivery.csv" contem dados de 25 entregas feitas 
## por uma empresa de delivery, contendo as seguintes variaveis
## identificacao da entrega (ID_Entrega)
## tempo gasto na entrega, em horas (Tempo_Entrega), 
## quantidade de itens entregues (N_Itens), 
## distancia ate o local de entrega, em Km (Distancia)

dados <- read.csv("dados_delivery.csv", header=T, dec=",", sep=";")
str(dados)   #Examina a estrutura do data frame "dados"

#--
#Analise descritiva (individual para cada variavel):
summary(dados) #Medidas de posicao (funciona com matrizes)
sd(dados)      #Desvio-padrao (nao funciona com matrizes)
apply(dados[,-c(1)], 2, sd)

par(mfrow=c(1,3)) #Divide a janela grafica em 1 linha e 3 colunas
hist(dados$Tempo_Entrega); hist(dados$N_Itens); hist(dados$Distancia) #Histogramas

win.graph()       #Abre uma janela grafica externa
par(mfrow=c(1,3)) #Divide a janela grafica em 1 linha e 3 colunas
boxplot(dados$Tempo_Entrega, ylab="Tempo de Entrega")
boxplot(dados$N_Itens, ylab="Numero de Itens")
boxplot(dados$Distancia, ylab="Distancia") 
savePlot(filename="boxplots_delivery", type="jpg")
dev.off()         #Fecha a janela grafica vigente

#--
#Graficos de dispersao:
win.graph()       #Abre uma janela grafica externa
par(mfrow=c(1,2)) #Divide a janela grafica em 1 linha e 2 colunas
plot(dados$N_Itens, dados$Tempo_Entrega)
with(dados, plot(Distancia, Tempo_Entrega))
savePlot(filename="dispersao_delivery", type="jpg")
dev.off()         #Fecha a janela grafica vigente

#--
#Calculo do coeficiente de correlacao linear amostral de Pearson:
cor(dados[,-1], method = "pearson") 

#--
#Teste de significancia para a correlacao (requer normalidade):
with(dados, qqnorm(Tempo_Entrega)); with(dados, qqline(Tempo_Entrega)); 
with(dados, qqnorm(N_Itens)); with(dados, qqline(N_Itens)); 
with(dados, qqnorm(Distancia)); with(dados, qqline(Distancia)); 

#Testes de normalidade:
#H_0:Os dados seguem uma distribuicao Normal
#H_1:Os dados nao seguem uma distribuicao Normal
with(dados, shapiro.test(Tempo_Entrega))
with(dados, shapiro.test(N_Itens))
with(dados, shapiro.test(Distancia)) 

#Teste para a correlacao H_0:rho=0 versus H_1:rho!=0
#Nao serao feitos pois dependem de normalidade.
#Nao um problema para ajustar o modelo de RLS, pois 
#nao 'e exigido/suposto normalidade para os dados. 
#Apenas nao faremos o teste 'parametrico' para significancia das correlacoes.

#--
## Regressao Linear: funcao lm() com variavel N_Itens (maior correlacao):
modelo1 <- lm(Tempo_Entrega ~ N_Itens, data=dados)
modelo1
summary(modelo1)
summary(modelo1)$r.squared   #Valor do R2
summary(modelo1)$sigma       #Valor do desvio-padrao do erro
anova(modelo1)

#--
## Regressao Linear: funcao lm() com variavel Distancia (maior correlacao):
modelo2 <- lm(Tempo_Entrega ~ Distancia, data=dados)
modelo2
summary(modelo2)
summary(modelo2)$r.squared   #Valor do R2
summary(modelo2)$sigma       #Valor do desvio-padrao do erro
anova(modelo2)

#--
## Analise do melhor modelo de RLS ajustado:
par(mfrow=c(1,2)) #Divide a janela grafica em 1 linha e 2 colunas
with(dados, plot(N_Itens, Tempo_Entrega, pch=16)); abline(modelo1, col="red", lwd=2)
text(x=10, y=75, labels = parse(text=paste0('R^2 ==', round(summary(modelo1)$r.squared*100, 2))))
text(x=10, y=70, labels = parse(text=paste0('S^2 ==', round(summary(modelo1)$sigma^2,2))))

with(dados, plot(Distancia, Tempo_Entrega, pch=16)); abline(modelo2, col="red", lwd=2)
text(x=400, y=75, labels = parse(text=paste0('R^2 ==', round(summary(modelo2)$r.squared*100, 2))))
text(x=400, y=70, labels = parse(text=paste0('S^2 ==', round(summary(modelo2)$sigma^2,2))))




print("Fim do script: Laboratorio 1, Parte B!")