
## Objetivos: construir intervalos de Confianca para os parâmetros
##            de uma populacao (media, proporcao e variancia)
###########################################################

#### limpando a memoria R: (aconselhavel)
rm(list=ls(all=TRUE))

#Instala e/ou chama os pacotes que permitem ler
# escrever planilhas no formato xlsx
# e calcula o IC para a variância
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
## Intervalo de Confiança para uma média  

## usando os dados brutos: funcao t.test()

t.test(dados1)$estimate   #estimativa pontual
t.test(dados1,conf.level=0.95)$conf.int  #Intervalo de Confiança


## Usando os dados resumidos (média, desvio-padrão e tamanho amostrais)

media <- mean(cereal$Calorias)
dp <- sd(cereal$Calorias)
n <- length(cereal$Calorias)

# Erro de estimacao com 100(1-alpha)% de confiança
alpha<-0.05   #IC de 95% de confiança
erro <- qt(df=n-1,(1-alpha/2)) * dp/sqrt(n)
LI <- media - erro  #Limite inferior
LS <- media + erro  #Limite superior
IC <- c(LI, LS)
IC   #exibe o IC para a media


#############################################
## Intervalo de Confiança para uma proporção  

## usando os dados brutos: funcao prop.test()
# Construir a tabela de frequencias absolutas (contagens) 
tab <- table(cereal$AdicaoV.e.M)
tab   
n <- length(cereal$AdicaoV.e.M)  #Tamanho da amostra

# IC para a proporção de cereias com nenhuma adição de vitaminas e mineirais
# prop.test(x,n,conf.level): x é a contagem do evento de interesse
#                            n é o tamanho da amostra 
#                            conf.level é o nível de confiança do intervalo

prop.test(x=8,n=n)$estimate   #Estimativa pontual
prop.test(x=8,n=n,conf.level=0.95)$conf.int  #Intervalo de confiança
IC <- round(prop.test(x=8,n=n,conf.level=0.95)$conf.int,3)



#############################################
## Intervalo de Confiança para uma variância  

## usando os dados brutos

s2 <- var(Hg.peixes)     #Variancia amostral   
n <- nrow(Hg.peixes)   #tamanho da amostra

IC.var <- confint.var(c(s2), c(n-1),level=0.95) #pacote Ecfun
round(IC.var,3)

#Conferindo cálculos
alpha <- 0.05
LI <- s2*(n-1)/qchisq(df=n-1,1-alpha/2)  #Limite inferior
LS <- s2*(n-1)/qchisq(df=n-1,alpha/2)    #Limite superior
IC <- c(LI, LS)
round(IC, 3)    #Intervalo de Confiança para a variancia

round(sqrt(IC),3)  #Intervalo de confiança para o desvio-padrao


#######################################
## Cálculo de tamanho de amostra 
## Estimação da Média Populacional

# Qual é o tamanho mínimo de amostra para se estimar 
# a idade média ao falar, em um intervalo de 95% de confiança,
# com um erro de estimação máximo de 1 mês? 
# Suponha que o desvio-padrão seja de 2 meses.

erro.max <- 1
sigma <- 2
alpha <-0.05

n <- (sigma*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima


#######################################
## Cálculo de tamanho de amostra 
## Estimação da Proporção Populacional

# Qual é o tamanho mínimo de amostra para se estimar 
# a proporção de pessoas imunizadas com uma nova vacina, 
# em um intervalo de 95% de confiança, com um erro de 
# estimação máximo de 0.10?
  
p <- 0.50    #quando não sabemos nada sobre a proporção a ser estimada
erro.max <- 0.10
alpha <- 0.05

n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima

# Se p vale, no mínimo, 0.70 (por exemplo)
p <- 0.70
n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
n
ceiling(n)   #arredonda para cima

erro.max<- 0.02   #comprimento do IC será de 0.02, no máximo
n <- (sqrt(p*(1-p))*qnorm(1-alpha/2)/erro.max)^2
ceiling(n)   #arredonda para cima
