
## Objetivo : mostrar os comandos do R para realizar
##            Teste Qui-quadrado de Pearson (homogeneidade,
##            associa��o e ader�ncia), teste exato de Fisher
##            teste de McNemar, c�lculo da raz�o de odds e do
##            risco relativo
###############################################################

#### limpando a memoria R: (aconselhavel)
rm(list=ls(all=TRUE))

if(!require(exact2x2)){install.packages("exact2x2")
  require(exact2x2)} #carrega ou instala o pacote exact2x2 
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")
  require(RVAideMemoire)} #carrega ou instala o pacote RVAideMemoire 
if(!require(reshape2)){install.packages("reshape2")
  require(reshape2)} #carrega ou instala o pacote RVAideMemoire 


###################################################
## Entrada de dados de uma tabela
## de classifica��o cruzada

## Ex1: n�mero de luvas de tr�s marcas que deixaram
##     e n�o deixaram passar v�rus durante um experimento
tab <-c(151,89,       #primeira linha
        134,106,      #segunda linha
        177,123)      #terceira linha
tabela <- matrix(tab,ncol=2,byrow=T,  #Entrada por linha
                 dimnames=list(c("A","B","C"),
                               c("Sim","Nao"))) 
tabela

## Teste Qui-quadrado de propor��es (amostras independentes)
## H0: as tr�s marcas t�m a mesma propor��o de luvas
##     que deixam passar o v�rus (permeabilidade)
## H1: ao menos uma das tr�s marcas tem uma
##     permeabilidade diferente das demais

saida <- chisq.test(tabela,correct=F)
saida

## Ex2: Estudo para verificar se a ingest�o de extrato de  guaran�
## tem efeito sobre a fadiga em pacientes tratados com quimioterapia  
tab <-c(21,11,     #Grupo guaran� (diminui��o da fadiga: sim e n�o)
         6,37)     #Grupo placebo (diminui��o da fadiga: sim e n�o)
tabela <- matrix(tab,ncol=2,byrow=T,
                 dimnames=list(c("Guaran�","Placebo"),
                               c("Sim","N�o")))
tabela
saida <- chisq.test(tabela,correct=F)
saida

## C�lculo do RR (risco relativo)
Risco1 <- tabela[1,1]/sum(tabela[1,])  #Grupo Guaran�
Risco2 <- tabela[2,1]/sum(tabela[2,])  #Grupo Placebo
RR <- Risco1/Risco2  
RR


#############################
# Lendo a planilha de dados:

#   Ex3: Associa��o entre toxoplasmose e acidente de 
#     tr�nsito em pessoas 
#     Flegr et al. (2009) "Increased incidence of traffic accidents 
#     in Toxoplasma-infected military drivers and protective effect RhD 
#     molecule revealed by a large-scale prospective cohort study", 
#     BMC Infectious Diseases, vol. 9, n. 72.

dados <- read.table("EST549_accident_toxo.csv", 
                    sep=";" , dec=",", header=TRUE)
str(dados)   #Examinando a estrutura da planilha de 
             #dados importada
View(dados)

## Convertendo a coluna de Rh em fator (vari�vel categ�rica)
dados$Rh <- factor(dados$Rh,levels=0:1,
                   labels=c("Positivo","Negativo"))
table(dados$Rh)

## Selecionando apenas pessoas com Rh negativo
dados.Rh.Neg <- subset(dados,dados$Rh=="Negativo")
table(dados.Rh.Neg$Rh)

### Tabela de classifica��o Cruzada
### Toxoplasma x Acidente
tabela <- table(dados.Rh.Neg$toxoplasma,
                dados.Rh.Neg$Acidente,
                dnn=list("Toxoplasma","Acidente"))
tabela

##Teste qui-quadrado
chisq.test(tabela,correct=F)

## C�lculo de odds ratio com tabela 2x2
tabela
exact2x2(tabela)$estimate       #Estimativa pontual da OR
exact2x2(tabela)$conf.int[1:2]  #Intervalo de Confian�a para a OR


########################################################
### Teste Exato de Fisher
###  Quando a aproxima��o pela distribui��o Qui-quadrado
###  n�o � adequada

## Ex4: Compara��o da propor��o de c�es infectados 
##      dentre os filhotes e adultos

tab <-c(7,3,       #Grupo de filhotes (infectado: sim e n�o)
        30,23)     #Grupo de adultos  (infectado: sim e n�o)
tabela <- matrix(tab,ncol=2,byrow=T,
                 dimnames=list(c("Filhotes","Adultos"),
                               c("Sim","N�o")))
tabela

chisq.test(tabela)  #Teste Qui-quadrado
chisq.test(tabela)$expected
fisher.test(tabela)  #Teste Exato de Fisher


###################################
### Teste Qui-quadrado de Ader�ncia

### Ex5: numero de medicamentos usados por idosos
dados <- read.table("EST549_Medicamentos_Idosos.txt",
                    header=T)
str(dados)

#Tabela com contagens observadas
tab <- table(dados$Numero)
tab
N <- sum(tab) ; N  #Numero de idosos
#Estimativa para o parametro da Poisson
lambda.chapeu <- mean(dados$Numero)  
lambda.chapeu

p.esperada <- dpois(x=0:13,lambda=lambda.chapeu)
p.esperada  #probabilidades esperadas sob H0
contagens.esp <- p.esperada*N #Contagens esperadas
contagens.esp

# Como os valores esperados para 0, 10, 11, 12 e 13 s�o muito
# pequenos, vamos reorganizar a tabela para n�o ter problemas
# com a aproxima��o pela distribui��o qui-quadrado
tab2<-c(sum(tab[1:2]),tab[3:10],sum(tab[11:14]))
tab2

#Faremos o  mesmo com as probabilidades esperadas
p.esperada2<-c(sum(p.esperada[1:2]),
               p.esperada[3:10],
               1-sum(p.esperada[1:10]))
p.esperada2 ; sum(p.esperada2)
# Conferindo as contagens esperadas
contagens.esp2 <- p.esperada2*N 
contagens.esp2

# Teste qui-quadrado
length(tab2)  #numero de caselas da tabela
saida <- chisq.test(x=tab2,p=p.esperada2)
saida$parameter  #graus de liberdade usados no teste

# Como usamos os dados para estimar a media da Poisson,
#  perdemos 01 grau de liberdade

#Valor-p como os graus de liberdade corretos
pchisq(df=saida$parameter-1,   #graus de liberdade corrigidos
       saida$statistic,        #estatistica de teste
       lower.tail = F)


#######################################
### Teste Qui-quadrado de McNemar
###  Quando as amostras s�o dependentes

## Ex6: Um estudo investiga a associa��o entre infarto do 
## mioc�rdio e a presen�a de diabetes entre os ind�genas navajos 
## americanos. Indiv�duos com epis�dios de infarto do mioc�rdio 
## foram emparelhados com indiv�duos sem a doen�a (144 pares). 
## Cada elemento do par foi investigado quanto � presen�a de diabetes.
## Coulehan et al (1986) "Acute Myocardial Infarction Among 
##   Navajo Indians", 1976-1983, American Journal of Public Health,
##   pp 412-214

mat<- matrix(c(9,37,16,82),ncol=2,byrow=T,
             dimnames=list(c("Diab�tico","N�o diab�tico"),
                           c("Diab�tico","N�o diab�tico")))
mat  # Linhas (Sem infarto) ; Colunas (Com infarto)
mcnemar.test(mat)  #Teste de McNemar


#########################################
### Teste Q de Cochran
###  Quando as amostras s�o dependentes e
###  e h� mais de dois grupos

## Ex7: Um grupo de 28 estudantes foi entrevistado
#  quanto ao tipo de bebida que consumiam: caf�,
#  ch� e/ou refrigerante do tipo cola. O objetivo
#  era investigar a homogeneidade das propor��es
#  de consumo das tr�s bebidas

dados<-read.table("EST549_Cochran_bebidas.txt",
                  header=T,row.names=1)
View(dados)
ftable(dados$Coffee,dados$Tea,dados$Cola,
       dnn=list("Coffee","Tea","Cola"))

#Preparando o conjunto de dados para 
# funcao que faz o teste Q de Cochran
dados<-as.matrix(dados)
names(dimnames(dados))[1] <- "estudante"
names(dimnames(dados))[2] <- "bebida"
novos.dados = melt(dados)
View(novos.dados)

# Teste Q de Cochran
cochran.qtest(value ~ bebida | estudante,
              data = novos.dados)

###################################################
