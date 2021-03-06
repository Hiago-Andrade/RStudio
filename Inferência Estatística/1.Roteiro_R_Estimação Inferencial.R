
## Objetivos: construir intervalos de Confianca para os par�metros
##            de uma populacao (media, proporcao e variancia)
###########################################################

#### limpando a memoria R: (aconselhavel)
rm(list=ls(all=TRUE))

#Instala e/ou chama os pacotes que permitem ler
# escrever planilhas no formato xlsx
# e calcula o IC para a vari�ncia
if(!require(readxl)){install.packages("readxl")
  require(readxl)}
if(!require(openxlsx)){install.packages("openxlsx")
  require(openxlsx)}
if(!require(Ecfun)){install.packages("Ecfun")
  require(Ecfun)}


###################
## Entrada de dados 

### Opcao 1: via teclado com a funcao scan()

dados1 <- scan()          #Via dados em arquivos texto (linha)

dados2 <- scan()         #Via dados em formato coluna


### Opcao 2: via arquivo de dados

cereal <- read_excel("EST549_cereal_data.xlsx",
                     sheet=1)  #Planilha formato Excel
str(cereal)  #examina a estrutura do conjunto de dados

Hg.peixes <- read.table("EST549_Hg_peixes.txt")
                      # Arquivo texto
str(Hg.peixes)


#########################################
## Intervalo de Confian�a para uma m�dia  

## usando os dados brutos: funcao t.test()

t.test(dados1)$estimate   #estimativa pontual
t.test(dados1,conf.level=0.95)$conf.int  #Intervalo de Confian�a


## Usando os dados resumidos (m�dia, desvio-padr�o e tamanho amostrais)

media <- mean(cereal$Calorias)
dp <- sd(cereal$Calorias)
n <- length(cereal$Calorias)

# Erro de estimacao com 100(1-alpha)% de confian�a
alpha<-0.05   #IC de 95% de confian�a
erro <- qt(df=n-1,(1-alpha/2)) * dp/sqrt(n)
LI <- media - erro  #Limite inferior
LS <- media + erro  #Limite superior
IC <- c(LI, LS)
IC   #exibe o IC para a media


#############################################
## Intervalo de Confian�a para uma propor��o  

## usando os dados brutos: funcao prop.test()
# Construir a tabela de frequencias absolutas (contagens) 
tab <- table(cereal$AdicaoV.e.M)
tab   
n <- length(cereal$AdicaoV.e.M)  #Tamanho da amostra

# IC para a propor��o de cereias com nenhuma adi��o de vitaminas e mineirais
# prop.test(x,n,conf.level): x � a contagem do evento de interesse
#                            n � o tamanho da amostra 
#                            conf.level � o n�vel de confian�a do intervalo

prop.test(x=8,n=n)$estimate   #Estimativa pontual
prop.test(x=8,n=n,conf.level=0.95)$conf.int  #Intervalo de confian�a
IC <- round(prop.test(x=8,n=n,conf.level=0.95)$conf.int,3)



#############################################
## Intervalo de Confian�a para uma vari�ncia  

## usando os dados brutos

s2 <- var(Hg.peixes)     #Variancia amostral   
n <- nrow(Hg.peixes)   #tamanho da amostra

IC.var <- confint.var(c(s2), c(n-1),level=0.95) #pacote Ecfun
round(IC.var,3)

#Conferindo c�lculos
alpha <- 0.05
LI <- s2*(n-1)/qchisq(df=n-1,1-alpha/2)  #Limite inferior
LS <- s2*(n-1)/qchisq(df=n-1,alpha/2)    #Limite superior
IC <- c(LI, LS)
round(IC, 3)    #Intervalo de Confian�a para a variancia

round(sqrt(IC),3)  #Intervalo de confian�a para o desvio-padrao


#######################################
## C�lculo de tamanho de amostra 
## Estima��o da M�dia Populacional

# Qual � o tamanho m�nimo de amostra para se estimar 
# a idade m�dia ao falar, em um intervalo de 95% de confian�a,
# com um erro de estima��o m�ximo de 1 m�s? 
# Suponha que o desvio-padr�o seja de 2 meses.

erro.max <- 1
sigma <- 2
alpha <-0.05

n <- (sigma*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima


#######################################
## C�lculo de tamanho de amostra 
## Estima��o da Propor��o Populacional

# Qual � o tamanho m�nimo de amostra para se estimar 
# a propor��o de pessoas imunizadas com uma nova vacina, 
# em um intervalo de 95% de confian�a, com um erro de 
# estima��o m�ximo de 0.10?
  
p <- 0.50    #quando n�o sabemos nada sobre a propor��o a ser estimada
erro.max <- 0.10
alpha <- 0.05

n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima

# Se p vale, no m�nimo, 0.70 (por exemplo)
p <- 0.70
n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima

erro.max<- 0.02   #comprimento do IC ser� de 0.02, no m�ximo
n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
ceiling(n)   #arredonda para cima
