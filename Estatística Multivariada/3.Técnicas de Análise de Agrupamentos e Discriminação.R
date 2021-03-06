############################################################################
## Disciplina M�todos de Estat�stica Multivariada 
## Especializacao em Estatistica - UFMG
## Aluno: Hiago Andrade Duarte
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Quest�o 1 Letra A ####################################
## Quest�o. Abra o arquivo vendedores.txt. Este arquivo cont�m informa��es sobre 44 funcion�rios 
## que trabalham em uma empresa. Para cada funcion�rio tem-se as notas � ele atribu�das pelo seu 
## desempenho nas vendas ( ) e lucros ( ) da empresa e pela sua capacidade de captar novos clientes 
## ( ). Tem-se ainda, as notas em quatro testes que medem sua habilidade escrita ( ), l�gica ( ), 
## social ( ) e matem�tica ( ), respectivamente.  As 3 primeiras vari�veis foram medidas na mesma 
## escala. As notas de matem�tica est�o medidas numa escala diferente das notas de escrita, l�gica 
## e social.

## Calcule as seguintes medidas descritivas de cada vari�vel: m�dia, mediana, desvio-padr�o, 
## coeficiente de varia��o, m�nimo e m�ximo. Comente brevemente os resultados.

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
## An�lise Descritiva
describe(vendedores[,-1], na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE, 
         trim= 0,type=3,check=TRUE,fast=TRUE, quant=c(.25,.75),IQR=TRUE)

##--
## Comentando os resultados

## As vari�veis Vendas, Lucro e Clientes apresentam comportamento semelahnte por apresentarem 
## m�dia, valor m�ximo e minimo parecidos, apesar da vari�vel Lucro possuir maior variancia.
## Escrita, L�gica e Social s�o muito parecidas tambem. Apenas matem�tica apresenta resusultados 
## diferentes. Isso se justifica por sua elevada m�dia, variancia e valor m�ximo quando comparada
## as variaveis Escrita, L�gica e Social.


############################ Quest�o 1 Letra B ####################################

## Fa�a o agrupamento dos vendedores usando as 7 vari�veis originais pelo m�todo Ward.D2. Estime 
## a regi�o de refer�ncia (intervalo) no qual o prov�vel n�mero de grupos "ideal" da parti��o 
## estaria inserido, avaliando o comportamento do n�vel de fus�o, o coeficiente de correla��o 
## intra-classe R2 e a estat�stica Pseudo F. Analise esses valores. (N�o � necess�rio colocar 
## os elementos que fazem parte dos grupos nesse item)


##--
## Estabelecendo a dist�ncia a ser usada no m�todo cluster (Euclidiana)
dist <- dist(vendedores[,-1], method = "euclidean")

##--
## M�todo de M�nima Vari�ncia de Ward no R � ward.D2
AC1 <- hclust(dist, method = "ward.D2")
ls(AC1)

##--
# Armazenando a raiz quadrada da dist�ncia de Ward de cada passo do agrupamento (dist_AC1)
dist_AC1<- AC1$height

##--
# Armazenando a Dist�ncia de Ward (n�vel de fus�o) em cada passo do agrupamento (AC1_dist_Ward)
AC1_dist_Ward <- AC1$height^2

##--
## Visualizando o Dendograma (num melhor formato)
plot(AC1, hang = -1, cex = 0.6, main = "Cluster dendrograma -
vendedores",labels=Funcionario)

##--
## C�lculo do Pseudo F (chamado de ch no NbClust) para cada n�mero de grupos de 1 a n-1.

AC2<- NbClust(vendedores[,-1], diss = NULL, distance = 
                "euclidean", min.nc = 1, max.nc = 44-1, method = "ward.D2", 
              index = "ch")

Pseudo_F <- AC2$All.index
list(Pseudo_F)

##--
## C�lculo do R2 (para cada n�mero de grupos de 1 a n-1)
ngrupos<- 1:(44-1)
c <-(44-ngrupos)/(ngrupos-1)
R2<- (100)*((Pseudo_F)/(c+Pseudo_F))

## Trocando NA por zero em R2 para o caso de no. de grupos=1
R2[is.na(R2)] <- 0

##--
# Resultado final: com n�vel de fus�o, R2 e Pseudo F em cada passo do agrupamento
ggrupos <- (44-1):1
Pseudo_F_g <- Pseudo_F[(44-1):1]
R2_g<- R2[(44-1):1]
Resultado <- data.frame(ggrupos,AC1_dist_Ward,dist_AC1,R2_g,Pseudo_F_g)

##--
## Gr�fico da dist�ncia de Ward para cada passo do agrupamento (N�vel de fus�o)
plot((44-1):1, AC1_dist_Ward, type="b", main="Gr�fico de 
Dist�ncias - N�vel de Fus�o - M�todo Ward.D2", 
     xlab="N�mero de Clusters",ylab="Dist�ncia de Ward")

## Gr�fico da raiz quadrada da dist�ncia de Ward para cada passo de agrupamento
plot((44-1):1, dist_AC1, type="b", main="Gr�fico de 
Dist�ncias - N�vel de Fus�o - M�todo Ward", xlab="N�mero 
de Clusters",ylab="Raiz Quadrada da Dist�ncia de Ward")

tail(Resultado,15)

##--
## N�mero de grupos para an�lise (k = 3,4,5,6,7,8). Agrupamentosn <3 trariam um R2 baixo abaixo 
## de 70%, j� utilizar >8 n�o traz um ganho de R2 significativo, e os valores de Pseudo_F caem 
## gradualmente. Nesse sentido,os agrupamentos 3 a 8, podemos observar pelo Pseudo_F que para 
## K = 6, 7 e 8 retornam valores menores do que com k = 3, 4 e 5.
## Dessa forma, iniciaria a an�lise de cluster com k = 4.

##--
## Gr�fico R2 x Clusters
plot(1:(44-1), R2, type="b",main="Gr�fico R2", 
     xlab="N�mero de Clusters",ylab="R2")

##Gr�fico Pseudo_F x Clusters
plot(1:(44-1), Pseudo_F, type="b", main="Gr�fico Pseudo_F", xlab="N�mero de Clusters",
     ylab="Pseudo_F")


############################ Quest�o 1 Letra C ####################################

## Como feito em sala de aula, com base nos valores encontrados em (b)  escolha o valor de k 
## (n�mero de grupos e correspondente parti��o dos dados) que achar mais apropriado. Fa�a um 
## dendograma da parti��o final e escreva a composi��o dos grupos.  Indique qual � o perfil geral 
## dos vendedores de cada um dos grupos. Justifique.  (Para identificar o perfil dos vendedores  
## fa�a no m�nimo uma an�lise descritiva dos grupos no que se refere as 7 vari�veis usadas na  
## parti��o dos dados. Pode usar  gr�ficos e outras t�cnicas que achar apropriadas).


##--
## N�mero de clusters escolhidos k = 4

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
## Em rela��o as vendas

## Os Clusters 4 apresentou a melhor venda em m�dia. O cluster 2 a pior. O Cluster 3 � o 
## maior com 16 vendedores.

##--
## Em rela��o ao Lucro

## Os Clusters 4 e 5 apresentam os Vendedores com maior lucro m�dio. O 
## Cluster 2 demonstra os vendedores menos lucrativos

##--
## Em rela��o os Clientes

## Os Clusters 4 e 5 se destacam com maiores capacidades de captar novos clientes. O Cluster 2 
## � o com pior desempenho em captar novos clientes.

##-
## Em rela��o as vari�veis: Escrita, Logica e Social

## Na vari�vel escrita, nota-se que o cluster 4 tem o melhor desempenho m�dio. Isso tambem ocorre 
## nas demais vari�veis

##-
## Em rela��o a matem�tica

## Seguindo como padr�o, o cluster 4 apresentou m�dia superior aos demais clusters



############################ Quest�o 1 Letra D ####################################

## Usando a solu��o de agrupamento que achou melhor em (c), indique quais s�o os 5 vendedores de 
## melhor desempenho e os 5 de pior desempenho. Justifique o crit�rio utilizado para a escolha.

##--
## Calculando a m�dia de cada vendedor

media = apply(vendedores_k_4[,-1],1,mean)     # n�mero 1 indica media por linhas
vendedores_k_4 = cbind(vendedores_k_4,media)

##-
## Pegando os vendedores do melhor (Cluster 4) e do pior cluster (Cluster 2)
vendedores_k_4_clust4 = vendedores_k_4[vendedores_k_4$K_4 == 4,]
vendedores_k_4_clust2 = vendedores_k_4[vendedores_k_4$K_4 == 2,]

## Melhores vendedores: Cluster 4, vendedores 8, 39, 35, 25 e 31
## Piores Vendedores: Cluster 2, vendedores 41, 44, 16, 23 e 2



############################ Quest�o 1 Letra E ####################################

## De acordo com o valor de k escolhido por voc� como o mais adequado em (c),  fa�a o agrupamento 
## pelo m�todo das k-M�dias com  as 7 vari�veis originais, usando: (e1)  as sementes do default 
## (nstart=1) do software R ; (e2)  as sementes iniciais que s�o os vetores de m�dias referentes 
## aos grupos formados por voc� anteriormente (de acordo com sua decis�o em (c)). D� os valores do 
## coeficiente de correla��o intra-classe R2 e a estat�stica Pseudo F da parti��o e compare esses 
## valores com os obtidos da parti��o em (c) . Escreva os grupos formados e os compare com os grupos
## formados pelo m�todo de Ward (do item (c)). Qual solu��o  lhe parece melhor? Justifique


##--
## K-M�dias com semente = 1

K_media_1 = kmeans (vendedores[,-1], 4, nstart =1)
K_media_1

R2_K_media_1 = K_media_1$betweenss/K_media_1$totss
gk = 5
Pseudo_F_K_media_1<- (44-gk)/(gk-1) * R2_K_media_1/(1-R2_K_media_1)

resultado_K_media_1<- data.frame(vendedores,K_media_1$cluster)

describeBy(resultado_K_media_1[,-1],group= resultado_K_media_1$K_media_1.cluster,mat=TRUE,fast=TRUE)



############################ Quest�o 1 Letra F ####################################

## Usando a solu��o de agrupamento resultante do k-M�dias que lhe pareceu a mais adequada em (e), 
## indique quais s�o os 5  vendedores de melhor desempenho e os 5 de pior desempenho. A solu��o � 
## semelhante �quela obtida pelo m�todo de Ward (do item (d)? Justifique.

##--
## Calculando a m�dia de cada vendedor

media2 = apply(resultado_K_media_1[,-1],1,mean)     # n�mero 1 indica media por linhas
resultado_K_media_1 = cbind(resultado_K_media_1,media)

## Os melhores vendedores e piores n�o alteraram muito, porem o cluster pertencente foi modificado



############################ Quest�o 1 Letra G ####################################

## De acordo com o valor de k escolhido por voc� como o mais adequado no item (c), fa�a o 
## agrupamento pelo m�todo Fuzzy usando os dados originais (as 7 vari�veis), par�metro fuzzy 
## m=2, dist�ncia Euclidiana ao quadrado e com  as sementes iniciais que s�o os vetores de m�dias 
## referentes aos grupos formados por voc� anteriormente (de acordo com sua decis�o em (e)). 
## Fa�a uma an�lise dos resultados (graus de pertin�ncia) e  d� os valores do coeficiente de 
## correla��o intra-classe R2 e a estat�stica Pseudo F da parti��o correspondente.  Compare esses 
## valores com os obtidos anteriormente no itens (c) e (e) . Escreva os grupos formados e os 
## compare com os grupos formados pelo m�todo de Ward e K- M�dias ( itens (c) e (e)). Qual solu��o
## lhe parece melhor? Justifique.


##--
## Realizar o m�todo Fuzzy 

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
## Os resultados s�o pr�ximos ao m�todo Ward.d2, Kmeans com semente Ward e Fuzzy com 
## semente Ward. Este resultado � esperado devido semente inicial das an�lises ser a mesma


############################ Quest�o 1 Letra H ####################################

## Usando a solu��o de agrupamento que achou melhor em (g), indique quais s�o os 5 vendedores 
## de melhor desempenho e os 5 de pior desempenho. Justifique o crit�rio utilizado para a escolha. 
## Compare a solu��o com as obtidas em (c) e (f).

##--
## Pela Tabela de R2 e PSeudo_F os melhores metodos seriam o Ward.d2 ou Kmeans com semente Ward.
## Ambos possuem o mesmo resultado.

## Os 5 melhores s�o: 8, 25, 31, 35 e 39.
## Os 5 piores s�o: 41, 44, 16, 23 e 2.



############################ Quest�o 1 Letra I ####################################

## Agora suponha que voc�, a partir do que aprendeu nessa an�lise de dados, tivesse que 
## sugerir um procedimento inicial para escolher os candidados para entrevista � uma vaga na 
## �rea de vendas dessa empresa. Qual seria sua sugest�o? Justifique claramente sua resposta.


## Observando os desempenhos de venda, lucro e cliente, o grupo do Cluster 4 seria o sugerigo 
## por mim