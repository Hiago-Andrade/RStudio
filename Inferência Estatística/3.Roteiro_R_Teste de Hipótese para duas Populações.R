
## Objetivos: mostrar os comandos do R para 
##            1) fazer testes de hipoteses para os parametros
##            de duas populacoes (medias, proporcoes e variancias)
##################################################################

#### limpando a memoria R: (aconselhavel)
rm(list=ls(all=TRUE))

##########################################################
######### Teste t-Student para comparacao de duas medias
#########

###################################
### Amostras Dependentes (Pareadas)

## Exemplo 1: Tempo para estacionar dois tipos de carros com 
##            barras de direção e raio de giro 
##            (Human Factors, 1962, pp. 375-380) 
##            Montgomery and Runger (5a. Edicão)

estacionar <- read.table("Tempo_estacionar_TH_duas amostras pareadas.txt",
                         header=TRUE)
str(estacionar)

boxplot(estacionar$Auto1 - estacionar$Auto2)
qqnorm(estacionar$Auto1 - estacionar$Auto2)  # Verificando a suposição de normalidade   
qqline(estacionar$Auto1 - estacionar$Auto2)  # das populações de onde vieram os dados
shapiro.test(estacionar$Auto1 - estacionar$Auto2)

saida <- t.test(estacionar$Auto1, 
                estacionar$Auto2, paired=TRUE)
saida

saida$p.value       # Exibindo o valor p do teste
saida$conf.int      # Exibindo o intervalo de confiança para a media
###


############################
### Amostras Independentes 

## Exemplo 2: Concentração de Arsenio na agua de 10 localidades 
##            urbanas e 10 localidades rurais 
##            (Exemplo 10.6, Montgomery and Runger)

arsenio <- read.table("Arsenio_agua_TH_duas amostras independentes.txt", 
                      header=TRUE)
str(arsenio)

boxplot(arsenio$Metropolitana, arsenio$Rural, names=names(arsenio))

qqnorm(arsenio$Metropolitana,pch=19)  # Verificando a suposição de normalidade   
qqline(arsenio$Metropolitana)         # da população de onde vieram os dados da maquina I
shapiro.test(arsenio$Metropolitana)   # Teste de normalidade dos dados

qqnorm(arsenio$Rural,pch=15) # Verificando a suposição de normalidade   
qqline(arsenio$Rural)        # da população de onde vieram os dados da maquina II
shapiro.test(arsenio$Rural)  # Teste de normalidade dos dados

## Considerando variância diferentes (teste t-Student-Welch)
saida1 <- t.test(arsenio$Metropolitana, arsenio$Rural)
saida1

## Considerando variância iguais (teste t-Student)
saida2 <- t.test(arsenio$Metropolitana, 
                arsenio$Rural, var.equal = T)
saida2

saida1$p.value    #p-valor do teste t-Student-Welch
saida2$p.value    #p-valor do teste t-Student

saida1$conf.int   #Intervalo de confiança
saida2$conf.int   #Intervalo de confiança
##############################################


######################################################
######### Teste F para comparacao de duas variancias
#########

saida <- var.test(arsenio$Metropolitana, 
                 arsenio$Rural)
saida


######################################################
######### Teste Z para comparacao de duas proporcoes
#########

## Exemplo: permeabilidade das luvas
n <- c(240,240)   #Vetor com tamanhos de amostra
x <- c(151,134)   #Vetor com número de sucessos

saida <- prop.test(x=x, n=n, correct = F)
saida
saida$p.value          #valor-p
sqrt(saida$statistic)  # valor ABSOLUTO da estatistica de teste (teste Z)
saida$conf.int         #intervalo de confiança para a diferença
####################################################
