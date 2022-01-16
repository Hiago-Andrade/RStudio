
## Objetivos: mostrar os comandos do R para 
##            1) fazer testes de hipoteses para os parametros
##            de uma populacao (media, proporcao e variancia)
##            2) verificar a suposicao de normalidade dos dados
###############################################################

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

### via teclado com a funcao scan()

acido.urico <- scan()  #Via dados em arquivos texto (linha)
14.6  15.3  15.6  12.7  14.1  14.6  
15.2  14.5  13.7  13.6  15.6  15.0  
12.8  13.9  13.5  15.3  12.7  16.3   14.9  15.5

miopia <- scan() 
1.8   9.0   8.1   7.3   2.8  
8.9   0.5   2.0   1.4   1.5 
1.4   1.0   7.8   2.1   0.8 
6.1   1.6   1.5   0.4   6.1 
2.5   4.1   2.8   1.1   3.1


### Opcao 2: via arquivo de dados

cereal <- read_excel("EST549_cereal_data.xlsx",
                     sheet=1)  #Planilha formato Excel
str(cereal)  #examina a estrutura do conjunto de dados

Hg.peixes <- read.table("EST549_Hg_peixes.txt")
                      # Arquivo texto
str(Hg.peixes)


####################################
## Teste de Hipóteses para uma média  

## usando os dados brutos: funcao t.test()

t.test(cereal$Calorias,mu=110)  #H0: mu = 110 x H1: mu != 110
#H0: mu = 110 x H1: mu < 110
t.test(cereal$Calorias,mu=110, alternative = "less")

## Usando os dados resumidos (média, desvio-padrão e tamanho amostrais)

media <- mean(acido.urico)
dp <- sd(acido.urico)
n <- length(acido.urico)
alpha <- 0.05   #nivel de significancia
mu.0 <- 15

# Calculando o valor da Estatistica de Teste
# para testar H0: mu = 15 x H1: mu != 15 
T.obs <- (media - mu.0)/(dp/sqrt(n))
T.obs

## Valor-p 
valor.p <- 2*pt(abs(T.obs), df=(n-1),
                lower.tail = F) #area acima de |T.obs|
valor.p
t.test(acido.urico,mu=15)$p.value  #Conferindo



########################################
## Teste de Hipóteses para uma proporção  

# TH para p, a proporção de cereais com nenhuma adição 
#     de vitaminas e minerais
# x é a contagem do evento de interesse
# n é o tamanho da amostra 
#          H0: p = 0.15  x  H1: p < 0.15

# Construir a tabela de frequencias absolutas (contagens) 
tab <- table(cereal$AdicaoV.e.M)
tab   
x <- 8  # 8  marcaas não adicionam vitaminas nem minerais  
n <- length(cereal$AdicaoV.e.M)  #Tamanho da amostra

p.0 <- 0.15    #valor de p sob H0
alpha <- 0.05  #nível de significância
p.chapeu <- x/n   #proporção amostral de cereais sem adição

#Estatística de teste
Z.obs <- (p.chapeu - p.0)/sqrt(p.0*(1-p.0)/n)
Z.obs

#Valor-p
valor.p <- pnorm(Z.obs)  #area abaixo Z.obs, pois o teste
                         # é unilateral esquerdo
valor.p

## Usando a funcao prop.test()  : COM CUIDADO
# A função prop.test() testa hipótese sobre uma proporção
# populacional, porém usando outra estatística de teste, e, claro,
# outra distribuição de referência

saida <- prop.test(x=8, n=n, p=0.15,  #H0: p = 0.15 x H1: p < 0.15
                   correct=F, alternative="less")
saida
## Estatística de teste para o teste Z das proporcoes
sqrt(saida$statistic)   #Se o teste é unilateral direito: OK!
-sqrt(saida$statistic)  #Se o teste é unilateral esquerdo:
                        # não esquecer de colocar o sinal negativo


########################################
## Teste de Hipóteses para uma variância  

## Concentração de Hg nos peixes
## H0: sigma2  = 0.15 
## HA: sigma2 != 0.15

## usando os dados brutos
s2 <- var(Hg.peixes$V1)     #Variancia amostral   
n <- nrow(Hg.peixes)     #tamanho da amostra
alpha <- 0.05
sigma2.0 <- 0.15

X2.obs <- (n-1)*s2/sigma2.0
X2.obs

mediana <- qchisq(df=n-1, 0.50) 
mediana  #calculando a mediana da distribuição de referência
         # para saber em qual cauda será calculado do valor-p

valor.p <- ifelse(X2.obs < mediana, 
                  2*pchisq(df=n-1,X2.obs),   #calcula valor-p na cauda inferior
                  2*pchisq(df=n-1,X2.obs,lower.tail=F)) #calcula valor-p na cauda superior
valor.p

# Se o teste fosse unilateral (esquerdo ou direito),
# basta dividir o valor-p
valor.p/2


#########################################
## Avaliando a suposição de "normalidade
##    dos dados"

boxplot(acido.urico)  #Box-plot
qqnorm(acido.urico)   #QQ-plot da normal padrão
qqline(acido.urico)   #desenha linha da normalidade no QQ-plot

## Teste de Shapiro-Wilk 
## H0: os dados vieram de uma distribuição que segue o modelo Normal
## HA: os dados não vieram de uma distribuição que segue o modelo Normal

shapiro.test(acido.urico)  # Teste de Shapiro-Wilk

# Teste de Kolmogorov-Smirnov (alternativa ao SW para amostras muito grandes)
ks.test(scale(acido.urico),"pnorm",0,1)  


####### Verificando a suposição de normalidade
boxplot(Hg.peixes)
qqnorm(Hg.peixes$V1)
qqline(Hg.peixes$V1)
shapiro.test(Hg.peixes$V1)

### Usar a transformacao raiz quadrada
boxplot(sqrt(Hg.peixes))
qqnorm(sqrt(Hg.peixes$V1))
qqline(sqrt(Hg.peixes$V1))
shapiro.test(sqrt(Hg.peixes$V1))


