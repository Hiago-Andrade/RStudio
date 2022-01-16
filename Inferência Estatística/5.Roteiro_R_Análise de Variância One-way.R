
#### limpando a mem???ria R:
rm(list=ls(all=TRUE))


#################################################
## ANOVA: an???lise de vari???ncias
## Para compara??????o das m???dias de v???rios grupos
#################################################

## Instala e carrega pacote necess???rio para 
## compara??????es m???ltiplas de m???dias
install.packages("agricolae", dependencies=TRUE)
library(agricolae)

## Descri??????o do problema
## O Lipitor ??? eficaz para redu??????o do LDL colesterol?

## O Lipitor (Laborat???rio Pfizer), utilizado para redu??????o do colesterol LDL, se tornou o medicamento mais vendido em todos os tempos, 
## com vendas atuais ultrapassando 13 bilh???es de d???lares por ano. 
## Os dados* apresentados no arquivo "dadosANOVA.xls" baseiam-se em 
## resultados apresentados em um memorando da Parke-Davis de David G. Orloff, 
## m???dico l???der da equipe de testes cl???nicos.  
## Os dados se referem ??? atorvastatina, para a qual Lipitor ??? o nome comercial. 
## A coluna Desvio_referencia apresenta a diferen???a entre o n???vel
## do LDL colesterol em rela??????o aos valores de refer???ncia (mg/dL). 
## Sendo assim, um valor negativo significa que houve redu??????o em 
## rela??????o ao valor de refer???ncia. 
## O estudo possui tr???s grupos de pacientes tomando doses diferentes 
## de atorvastatina (10 mg, 20 mg e 80 mg) e um grupo que tomou o 
## placebo do medicamento (coluna Grupo).

## *Dados retirados do livro "Introdu??????o ??? Estat???stica" (Mario F. Triola),
## Editora LTC, 11??? Edi??????o. 
## O autor comenta que pediu os dados originais ao Laborat???rio Pfizer, 
## que n???o atendeu ??? solicita??????o.

#####################
## Lendo banco de dados dos pacientes com diabetes
library(readxl)     
dados <- read_excel("dadosANOVA.xls")
str(dados)   #Examina a estrutura do conjunto de dados lido

###########
## An???lise Explorat???ria da vari???vel Desvio

boxplot(dados$Desvio_referencia ~ dados$Grupo)
table(dados$Grupo)

###########
## An???lise de Variancia:

## Teste de Normalidade dos dados
#   H0: os dados vieram de uma popula??????o que segue a Distribui??????o Normal
# Graficos de NOrmalidade
qqnorm(dados$Desvio_referencia[dados$Grupo=="Grupo10"])  #Grupo 10
qqline(dados$Desvio_referencia[dados$Grupo=="Grupo10"])  #Grupo 10
shapiro.test(dados$Desvio_referencia[dados$Grupo=="Grupo10"])  #Teste de Shapiro-Wilk

qqnorm(dados$Desvio_referencia[dados$Grupo=="Grupo20"])  #Grupo 20
qqline(dados$Desvio_referencia[dados$Grupo=="Grupo20"])  #Grupo 20
shapiro.test(dados$Desvio_referencia[dados$Grupo=="Grupo20"])  #Teste de Shapiro-Wilk

qqnorm(dados$Desvio_referencia[dados$Grupo=="Grupo80"])  #Grupo 80
qqline(dados$Desvio_referencia[dados$Grupo=="Grupo80"])  #Grupo 80
shapiro.test(dados$Desvio_referencia[dados$Grupo=="Grupo80"])  #Teste de Shapiro-Wilk

qqnorm(dados$Desvio_referencia[dados$Grupo=="placebo"])  #Placebo
qqline(dados$Desvio_referencia[dados$Grupo=="placebo"])  #Placebo
shapiro.test(dados$Desvio_referencia[dados$Grupo=="placebo"])  #Teste de Shapiro-Wilk

## Teste da hip???tese de vari???ncias homog???neas nos quatro grupos
#   H0: os grupos t???m vari???ncias iguais
bartlett.test(dados$Desvio_referencia ~ dados$Grupo)

## A hip???tese de vari???ncia constante n???o foi satisfeita.
## H??? maneiras de tentar contornar este problema e ainda usar
## a t???cnica da ANOVA. Uma delas ??? usar pesos diferentes para
## as observa??????es. Outra ??? usar uma t???cnica n???o-param???trica.
## Como n???o ??? o objeto desse curso, vamos seguir considerando que 
## a as duas suposi??????es necess???rias para Analise de Variancia est???o  vamos realiz???-la.
## satisfeitas

## ANOVA
# H0: as medias de todos os grupos s???o iguais
saida <- aov(Desvio_referencia ~ Grupo, data=dados)
anova(saida)   #Tabela ANOVA  #Teste F rejeita a H0

## Que pares de medias sao diferentes?
## Proximo passo: 
## Encontrar a diferen???a entre as medias dos grupos

#Teste de Tukey
TukeyHSD(saida,ordere=T)
plot(TukeyHSD(saida,ordered=T))  #Grafico dos IC???s

#Teste de Fisher
a<-LSD.test(saida,"Grupo", group=F)
a

#Correcao de Bonferroni
b<-LSD.test(saida,"Grupo", group=F, p.adj = "bonferroni")
b
