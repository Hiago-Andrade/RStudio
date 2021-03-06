############################################################################
## Disciplina M�todos de Estat�stica Multivariada 
## Especializacao em Estatistica - UFMG
## Aluno: Hiago Andrade Duarte
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Quest�o 1 Letra A ####################################
## Fa�a uma an�lise fatorial por matriz de correla��o (sem utilizar a vari�vel idade) usando o 
## m�todo de componentes principais para estimar o n�mero de fatores e as cargas fatoriais 
## correspondentes. Estime o valor de m, usando a regra dos autovalores maiores ou iguais a 1 
## (Regra de Kaiser). D� a vari�ncia explicada de cada fator de sua solu��o e a vari�ncia explicada
## acumulada. Escreva a composi��o dos fatores (ou seja as vari�veis alocadas em cada fator) e 
## interprete os fatores de sua solu��o de acordo com o valor m estimado. 

##-- 
## Ler conjunto de dados:
FSAUDE <- read.table("FSAUDE.txt",
                     header=TRUE)
str(FSAUDE)

##-- 
## Instalando o pacote:
if(!require(psych)){install.packages("psych")
  require(psych)} 

##--
## An�lise Descritiva
describe(FSAUDE[,-1], na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE, 
         trim= 0,type=3,check=TRUE,fast=TRUE, quant=c(.25,.75),IQR=TRUE)

##-
## Matriz de Correla��o
cor(FSAUDE[,-1])

##-
## Comando para realizar a An�lise de componentes princicipais m=13
pca.FSAUDE <- principal(FSAUDE[ ,-1], nfactors=13, n.obs=40,rotate='none', 
                        scores=FALSE)
pca.FSAUDE

##-
## Explica��o linhas da matriz dos fatores (PC's)

## PROPORTION VAR: � a propor��o de Vari�ncia Total Explicada de cada Fator em rela��o a Vari�ncia Total considerando todas as p=7 vari�veis. � igual ao autovalor correspondente da matriz de correla��o das vari�veis
## PROPORTION EXPLAINED: � a propor��o de Vari�ncia Total Explicada de cada Fator em rela��o a soma das Vari�ncias Explicadas pelos m Fatores inclu�dos no modelo. No caso em que m=7 as duas medidas s�o iguais. O mesmo n�o ocorre quando m � diferente de p.
## SS LOADINGS: � a soma de quadrados de todos os valores das cargas fatorias relativas a cada Fator.

##-
## Fazendo scree plot
scree(FSAUDE[,-1],factors=FALSE,pc=TRUE,main="Scree plot",
      hline=NULL,add=FALSE)

##-
## Verificando valor de m An�lise Fatorial(AF) e Explica��es das variancias de cada fator

Var_expl = c(6.43, 2.25, 1.33, 0.94, 0.71, 0.55, 0.28, 0.19, 0.14, 0.09, 0.05, 0.04, 0) # Valores ss loadings
Perc_Var_expl = Var_expl/sum(Var_expl) # Valores Proportion Var
Perc_Acuml_Var_expl = cumsum(Perc_Var_expl) # Valores Cumulative Var

Tab_autov_mat_correl <- data.frame(Var_expl, Perc_Var_expl,Perc_Acuml_Var_expl)
names(Tab_autov_mat_correl) <- c("Explica��o Variavel(Autovalores)", "% Explica��o Variavel", "% Acumulada Explica��o Variavel")

## Pelo crit�rio de Kaiser, m deve ser igual a 3 po�s � o numero de autovetores maiores que 1
## No entanto, para explicar um percentual >90%, m deve ser igual a 6

##-
## Gerando a solu��o inicial de AF com o n�mero de fatores (m=3) de Kaiser, sem rota��o

AF1.corr <- principal(FSAUDE[ ,-1], nfactors=3, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF1.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Interpreta��o dos fatores para m=3

##  PC1 est� mais correlacionada com as variaveis PESO, CINTURA, IMC e BRACO. PC2 � mais correlacionada com PERNA e PC3 com TX_PULSACAO 

## As vari�veis DIASTOLICA e PULSO tem graus de correla��o parecidos para PC1 e PC2

## A vari�vel ALTURA tem graus de correla��o parecidos para PC2 e PC3


############################ Quest�o 1 Letra B ####################################
## Fa�a uma rota��o ortogonal Varimax dos fatores encontrados em (a). 

##-
## Rota��o Varimax

AF2.corr_rot <- principal(FSAUDE[ ,-1], nfactors=3, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF2.corr_rot, digits=4, cutoff=.0,sort=FALSE)



############################ Quest�o 1 Letra B1 ####################################
## Escreva a composi��o dos fatores (ou seja as vari�veis alocadas em cada fator) e interprete 
## os fatores da  solu��o rotacionada. Qual solu��o (sem ou com rota��o) em termos de interpreta��o 
## lhe parece melhor?  Justifique.  

##-
## RC1 continua mais correlacionada com as variaveis PESO, CINTURA, IMC e BRACO. J� RC2 � mais correlacionada com ALTURA e PERNA e RC3 com TX_PULSACAO  e DIASTOLICA

## Em termos de interpreta��o, me parece melhor a solu��o com rota��o pelos motivos de n�o terem 
## vari�veis com graus de covariancia parecidos entre os fatores e o percentual acumulado de 
## explica��o ser semelhante entre as duas solu��es



############################ Quest�o 1 Letra B2 ####################################
## D� as comunalidades, vari�ncias espec�ficas e complexidades estimadas (antes da rota��o e 
## depois da rota��o ortogonal).  Compare  esses valores com os obtidos  (a). Existe alguma 
## diferen�a?

##-
## A comunalidade(h2) e a vari�ncia espec�fica(u2) s�o semelhantes para as duas solu��es
## J� a complexidade � diferente sendo que, quanto mais pr�ximo de 1 melhor � a solu��o da an�lise fatorial. Na rota��o temos 3 variaveis com complexidade acima de 2, sem rota��o temos 2 variaveis apenas;



############################ Quest�o 1 Letra B3 ####################################
## Considerando suas respostas em (b1) e (b2), qual solu��o lhe parece melhor? 
## (com ou sem rota��o?). Justifique.

##-
## Me parece melhor a solu��o sem rota��o, por ter complexidades mais pr�ximas de 1 e por ter variaveis bem correlacionadas nos tres fatores



############################ Quest�o 1 Letra C ####################################
## Agora adicione um fator � sua solu��o encontrada em (a) (solu��o sem rota��o ortogonal). 
## Fa�a a rota��o varimax dessa solu��o. Responda o item (b) novamente considerando essa nova
## solu��o. Compare essa novasolu��o com as obtidas em (a) e (b).

##-
## Adicionando um fator

AF3.corr <- principal(FSAUDE[ ,-1], nfactors=4, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF3.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Realizando rota��o varimax

AF4.corr_rot <- principal(FSAUDE[ ,-1], nfactors=4, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF4.corr_rot, digits=4, cutoff=.0, sort=FALSE)

##-
## Comparando resultados

## Para m=4, me parece melhor a solu��o com rota��o, por ter complexidades mais pr�ximas de 1 e por ter variaveis bem correlacionadas nos quatro fatores



############################ Quest�o 1 Letra D ####################################
## Agora adicione dois fatores � sua solu��o encontrada em (a) (solu��o sem rota��o ortogonal). 
## Fa�a a rota��o varimax dessa solu��o. Responda o item (b) novamente considerando essa nova 
## solu��o. Compare essa nova solu��o com as obtidas em (a),  (b) e (c).

##-
## Adicionando mais um fator

AF4.corr <- principal(FSAUDE[ ,-1], nfactors=5, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF4.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Realizando rota��o varimax

AF5.corr_rot <- principal(FSAUDE[ ,-1], nfactors=5, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF5.corr_rot, digits=4, cutoff=.0, sort=FALSE)

##-
## Comparando resultados

## Para m=5, me parece melhor a solu��o com rota��o, por ter complexidades mais pr�ximas de 1 e por ter variaveis bem correlacionadas nos cinco  fatores



############################ Quest�o 1 Letra E ####################################
## Gere os escores dos fatores da solu��o que considerou a melhor (entre os itens (a), (b), (c), 
## (d)). Mostre os pesos de pondera��o que s�o usados para os c�lculos dos escores gerados. 
## Escreva a equa��o correspondente ao c�lculo dos escores do primeiro fator.


##-
## Comando para visualizar os escores dos elementos amostrais (m=5) rota��o varimax

print(AF5.corr_rot$scores, digits=4, cutoff=.0, sort=FALSE)



############################ Quest�o 1 Letra G ####################################
## Calcule os valores do �ndice KMO para cada uma das 13 vari�veis envolvidas nessa an�lise 
## fatorial. Inteprete os resultados.


##-
## Comando para c�lculo da medida descritiva KMO

KMO(r = FSAUDE [ , -1])

## KMO's pr�ximos de 1 h� boas chance de ter agrupamento entre as variaveis.