############################################################################
## Disciplina Métodos de Estatística Multivariada 
## Especializacao em Estatistica - UFMG
## Aluno: Hiago Andrade Duarte
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Questão 1 Letra A ####################################
## Questão. Abra o arquivo vendedores.txt. Este arquivo contém informações sobre 44 funcionários 
## que trabalham em uma empresa. Para cada funcionário tem-se as notas à ele atribuídas pelo seu 
## desempenho nas vendas ( ) e lucros ( ) da empresa e pela sua capacidade de captar novos clientes 
## ( ). Tem-se ainda, as notas em quatro testes que medem sua habilidade escrita ( ), lógica ( ), 
## social ( ) e matemática ( ), respectivamente.  As 3 primeiras variáveis foram medidas na mesma 
## escala. As notas de matemática estão medidas numa escala diferente das notas de escrita, lógica 
## e social.

## Calcule as seguintes medidas descritivas de cada variável: média, mediana, desvio-padrão, 
## coeficiente de variação, mínimo e máximo. Comente brevemente os resultados.

##-- 
## Ler conjunto de dados:
vendedores <- read.table("vendedores.txt",
                         header=TRUE)
str(vendedores)

##-- 
## Instalando o pacote:
if(!require(psych)){install.packages("psych")
  require(psych)} 

if(!require(NbClust)){install.packages("NbClust")
  require(NbClust)} 

if(!require(ppclust)){install.packages("ppclust")
  require(ppclust)} 

##--
## Análise Descritiva
describe(vendedores[,-1], na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE, 
         trim= 0,type=3,check=TRUE,fast=TRUE, quant=c(.25,.75),IQR=TRUE)

##--
## Comentando os resultados

## As variáveis Vendas, Lucro e Clientes apresentam comportamento semelahnte por apresentarem 
## média, valor máximo e minimo parecidos, apesar da variável Lucro possuir maior variancia.
## Escrita, Lógica e Social são muito parecidas tambem. Apenas matemática apresenta resusultados 
## diferentes. Isso se justifica por sua elevada média, variancia e valor máximo quando comparada
## as variaveis Escrita, Lógica e Social.


############################ Questão 1 Letra B ####################################

## Faça o agrupamento dos vendedores usando as 7 variáveis originais pelo método Ward.D2. Estime 
## a região de referência (intervalo) no qual o provável número de grupos "ideal" da partição 
## estaria inserido, avaliando o comportamento do nível de fusão, o coeficiente de correlação 
## intra-classe R2 e a estatística Pseudo F. Analise esses valores. (Não é necessário colocar 
## os elementos que fazem parte dos grupos nesse item)


##--
## Estabelecendo a distância a ser usada no método cluster (Euclidiana)
dist <- dist(vendedores[,-1], method = "euclidean")

##--
## Método de Mínima Variância de Ward no R é ward.D2
AC1 <- hclust(dist, method = "ward.D2")
ls(AC1)

##--
# Armazenando a raiz quadrada da distância de Ward de cada passo do agrupamento (dist_AC1)
dist_AC1<- AC1$height

##--
# Armazenando a Distância de Ward (nível de fusão) em cada passo do agrupamento (AC1_dist_Ward)
AC1_dist_Ward <- AC1$height^2

##--
## Visualizando o Dendograma (num melhor formato)
plot(AC1, hang = -1, cex = 0.6, main = "Cluster dendrograma -
vendedores",labels=Funcionario)

##--
## Cálculo do Pseudo F (chamado de ch no NbClust) para cada número de grupos de 1 a n-1.

AC2<- NbClust(vendedores[,-1], diss = NULL, distance = 
                "euclidean", min.nc = 1, max.nc = 44-1, method = "ward.D2", 
              index = "ch")

Pseudo_F <- AC2$All.index
list(Pseudo_F)

##--
## Cálculo do R2 (para cada número de grupos de 1 a n-1)
ngrupos<- 1:(44-1)
c <-(44-ngrupos)/(ngrupos-1)
R2<- (100)*((Pseudo_F)/(c+Pseudo_F))

## Trocando NA por zero em R2 para o caso de no. de grupos=1
R2[is.na(R2)] <- 0

##--
# Resultado final: com nível de fusão, R2 e Pseudo F em cada passo do agrupamento
ggrupos <- (44-1):1
Pseudo_F_g <- Pseudo_F[(44-1):1]
R2_g<- R2[(44-1):1]
Resultado <- data.frame(ggrupos,AC1_dist_Ward,dist_AC1,R2_g,Pseudo_F_g)

##--
## Gráfico da distância de Ward para cada passo do agrupamento (Nível de fusão)
plot((44-1):1, AC1_dist_Ward, type="b", main="Gráfico de 
Distâncias - Nível de Fusão - Método Ward.D2", 
     xlab="Número de Clusters",ylab="Distância de Ward")

## Gráfico da raiz quadrada da distância de Ward para cada passo de agrupamento
plot((44-1):1, dist_AC1, type="b", main="Gráfico de 
Distâncias - Nível de Fusão - Método Ward", xlab="Número 
de Clusters",ylab="Raiz Quadrada da Distância de Ward")

tail(Resultado,15)

##--
## Número de grupos para análise (k = 3,4,5,6,7,8). Agrupamentosn <3 trariam um R2 baixo abaixo 
## de 70%, já utilizar >8 não traz um ganho de R2 significativo, e os valores de Pseudo_F caem 
## gradualmente. Nesse sentido,os agrupamentos 3 a 8, podemos observar pelo Pseudo_F que para 
## K = 6, 7 e 8 retornam valores menores do que com k = 3, 4 e 5.
## Dessa forma, iniciaria a análise de cluster com k = 4.

##--
## Gráfico R2 x Clusters
plot(1:(44-1), R2, type="b",main="Gráfico R2", 
     xlab="Número de Clusters",ylab="R2")

##Gráfico Pseudo_F x Clusters
plot(1:(44-1), Pseudo_F, type="b", main="Gráfico Pseudo_F", xlab="Número de Clusters",
     ylab="Pseudo_F")


############################ Questão 1 Letra C ####################################

## Como feito em sala de aula, com base nos valores encontrados em (b)  escolha o valor de k 
## (número de grupos e correspondente partição dos dados) que achar mais apropriado. Faça um 
## dendograma da partição final e escreva a composição dos grupos.  Indique qual é o perfil geral 
## dos vendedores de cada um dos grupos. Justifique.  (Para identificar o perfil dos vendedores  
## faça no mínimo uma análise descritiva dos grupos no que se refere as 7 variáveis usadas na  
## partição dos dados. Pode usar  gráficos e outras técnicas que achar apropriadas).


##--
## Número de clusters escolhidos k = 4

K_4<- cutree(AC1,k=4)
K_4

## Dendograma
## Fiz o Dendograma K=4
plot(AC1, hang = -1, cex = 0.6, main = "Cluster dendrogram -
k=4",labels= vendedores$Funcionario)
rect.hclust(AC1,k=4,border="red")

## Calssificando cada vendedor de acordo com seu cluster
vendedores_k_4<- cbind(vendedores,K_4)
vendedores_k_4

describeBy(vendedores_k_4[,-1],group=K_4,mat=TRUE,fast=TRUE)

##--
## Em relação as vendas

## Os Clusters 4 apresentou a melhor venda em média. O cluster 2 a pior. O Cluster 3 é o 
## maior com 16 vendedores.

##--
## Em relação ao Lucro

## Os Clusters 4 e 5 apresentam os Vendedores com maior lucro médio. O 
## Cluster 2 demonstra os vendedores menos lucrativos

##--
## Em relação os Clientes

## Os Clusters 4 e 5 se destacam com maiores capacidades de captar novos clientes. O Cluster 2 
## é o com pior desempenho em captar novos clientes.

##-
## Em relação as variáveis: Escrita, Logica e Social

## Na variável escrita, nota-se que o cluster 4 tem o melhor desempenho médio. Isso tambem ocorre 
## nas demais variáveis

##-
## Em relação a matemática

## Seguindo como padrão, o cluster 4 apresentou média superior aos demais clusters



############################ Questão 1 Letra D ####################################

## Usando a solução de agrupamento que achou melhor em (c), indique quais são os 5 vendedores de 
## melhor desempenho e os 5 de pior desempenho. Justifique o critério utilizado para a escolha.

##--
## Calculando a média de cada vendedor

media = apply(vendedores_k_4[,-1],1,mean)     # número 1 indica media por linhas
vendedores_k_4 = cbind(vendedores_k_4,media)

##-
## Pegando os vendedores do melhor (Cluster 4) e do pior cluster (Cluster 2)
vendedores_k_4_clust4 = vendedores_k_4[vendedores_k_4$K_4 == 4,]
vendedores_k_4_clust2 = vendedores_k_4[vendedores_k_4$K_4 == 2,]

## Melhores vendedores: Cluster 4, vendedores 8, 39, 35, 25 e 31
## Piores Vendedores: Cluster 2, vendedores 41, 44, 16, 23 e 2



############################ Questão 1 Letra E ####################################

## De acordo com o valor de k escolhido por você como o mais adequado em (c),  faça o agrupamento 
## pelo método das k-Médias com  as 7 variáveis originais, usando: (e1)  as sementes do default 
## (nstart=1) do software R ; (e2)  as sementes iniciais que são os vetores de médias referentes 
## aos grupos formados por você anteriormente (de acordo com sua decisão em (c)). Dê os valores do 
## coeficiente de correlação intra-classe R2 e a estatística Pseudo F da partição e compare esses 
## valores com os obtidos da partição em (c) . Escreva os grupos formados e os compare com os grupos
## formados pelo método de Ward (do item (c)). Qual solução  lhe parece melhor? Justifique


##--
## K-Médias com semente = 1

K_media_1 = kmeans (vendedores[,-1], 4, nstart =1)
K_media_1

R2_K_media_1 = K_media_1$betweenss/K_media_1$totss
gk = 5
Pseudo_F_K_media_1<- (44-gk)/(gk-1) * R2_K_media_1/(1-R2_K_media_1)

resultado_K_media_1<- data.frame(vendedores,K_media_1$cluster)

describeBy(resultado_K_media_1[,-1],group= resultado_K_media_1$K_media_1.cluster,mat=TRUE,fast=TRUE)



############################ Questão 1 Letra F ####################################

## Usando a solução de agrupamento resultante do k-Médias que lhe pareceu a mais adequada em (e), 
## indique quais são os 5  vendedores de melhor desempenho e os 5 de pior desempenho. A solução é 
## semelhante àquela obtida pelo método de Ward (do item (d)? Justifique.

##--
## Calculando a média de cada vendedor

media2 = apply(resultado_K_media_1[,-1],1,mean)     # número 1 indica media por linhas
resultado_K_media_1 = cbind(resultado_K_media_1,media)

## Os melhores vendedores e piores não alteraram muito, porem o cluster pertencente foi modificado



############################ Questão 1 Letra G ####################################

## De acordo com o valor de k escolhido por você como o mais adequado no item (c), faça o 
## agrupamento pelo método Fuzzy usando os dados originais (as 7 variáveis), parâmetro fuzzy 
## m=2, distância Euclidiana ao quadrado e com  as sementes iniciais que são os vetores de médias 
## referentes aos grupos formados por você anteriormente (de acordo com sua decisão em (e)). 
## Faça uma análise dos resultados (graus de pertinência) e  dê os valores do coeficiente de 
## correlação intra-classe R2 e a estatística Pseudo F da partição correspondente.  Compare esses 
## valores com os obtidos anteriormente no itens (c) e (e) . Escreva os grupos formados e os 
## compare com os grupos formados pelo método de Ward e K- Médias ( itens (c) e (e)). Qual solução
## lhe parece melhor? Justifique.


##--
## Realizar o método Fuzzy 

library(fcm)

if(!require(fclust)){install.packages("fclust")
  require(fclust)} 

if(!require(cluster)){install.packages("cluster")
  require(cluster)} 

if(!require(dplyr)){install.packages("dplyr")
  require(dplyr)} 

if(!require(factoextra)){install.packages("factoextra")
  require(factoextra)} 

fvendedores<- vendedores[,-1]
fcm1<- fcm(fvendedores,semente_ward5,m=2, dmetric="sqeuclidean",nstart=6, iter.max=10000, con.val=1e-09, 
           fixcent=TRUE, fixmemb=FALSE, stand=FALSE)
summary(fcm1)
ls(fcm1)
fcm1$v0
fcm1$v
Grupo <- fcm1$cluster
resultado_fuzzy <- data.frame(fvendedores[,1],fcm1$u,Grupo)
head(resultado_fuzzy,2)
print (resultado_fuzzy)

##--
## Calculando PSeudo_F e R2
R2_fcm1<- fcm1$sumsqrs$between.ss/fcm1$sumsqrs$tot.ss
g<-4
Pseudo_F_fcm1<- (44-g)/(g-1) * R2_fcm1/(1-R2_fcm1)
R2_fcm

##--
## Os resultados são próximos ao método Ward.d2, Kmeans com semente Ward e Fuzzy com 
## semente Ward. Este resultado é esperado devido semente inicial das análises ser a mesma


############################ Questão 1 Letra H ####################################

## Usando a solução de agrupamento que achou melhor em (g), indique quais são os 5 vendedores 
## de melhor desempenho e os 5 de pior desempenho. Justifique o critério utilizado para a escolha. 
## Compare a solução com as obtidas em (c) e (f).

##--
## Pela Tabela de R2 e PSeudo_F os melhores metodos seriam o Ward.d2 ou Kmeans com semente Ward.
## Ambos possuem o mesmo resultado.

## Os 5 melhores são: 8, 25, 31, 35 e 39.
## Os 5 piores são: 41, 44, 16, 23 e 2.



############################ Questão 1 Letra I ####################################

## Agora suponha que você, a partir do que aprendeu nessa análise de dados, tivesse que 
## sugerir um procedimento inicial para escolher os candidados para entrevista à uma vaga na 
## área de vendas dessa empresa. Qual seria sua sugestão? Justifique claramente sua resposta.


## Observando os desempenhos de venda, lucro e cliente, o grupo do Cluster 4 seria o sugerigo 
## por mim