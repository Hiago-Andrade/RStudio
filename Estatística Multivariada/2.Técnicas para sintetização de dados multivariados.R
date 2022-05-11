############################################################################
## Disciplina Métodos de Estatística Multivariada 
## Especializacao em Estatistica - UFMG
## Aluno: Hiago Andrade Duarte
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Questão 1 Letra A ####################################
## Faça uma análise fatorial por matriz de correlação (sem utilizar a variável idade) usando o 
## método de componentes principais para estimar o número de fatores e as cargas fatoriais 
## correspondentes. Estime o valor de m, usando a regra dos autovalores maiores ou iguais a 1 
## (Regra de Kaiser). Dê a variância explicada de cada fator de sua solução e a variância explicada
## acumulada. Escreva a composição dos fatores (ou seja as variáveis alocadas em cada fator) e 
## interprete os fatores de sua solução de acordo com o valor m estimado. 

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
## Análise Descritiva
describe(FSAUDE[,-1], na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE, 
         trim= 0,type=3,check=TRUE,fast=TRUE, quant=c(.25,.75),IQR=TRUE)

##-
## Matriz de Correlação
cor(FSAUDE[,-1])

##-
## Comando para realizar a Análise de componentes princicipais m=13
pca.FSAUDE <- principal(FSAUDE[ ,-1], nfactors=13, n.obs=40,rotate='none', 
                        scores=FALSE)
pca.FSAUDE

##-
## Explicação linhas da matriz dos fatores (PC's)

## PROPORTION VAR: É a proporção de Variância Total Explicada de cada Fator em relação a Variância Total considerando todas as p=7 variáveis. É igual ao autovalor correspondente da matriz de correlação das variáveis
## PROPORTION EXPLAINED: É a proporção de Variância Total Explicada de cada Fator em relação a soma das Variâncias Explicadas pelos m Fatores incluídos no modelo. No caso em que m=7 as duas medidas são iguais. O mesmo não ocorre quando m é diferente de p.
## SS LOADINGS: é a soma de quadrados de todos os valores das cargas fatorias relativas a cada Fator.

##-
## Fazendo scree plot
scree(FSAUDE[,-1],factors=FALSE,pc=TRUE,main="Scree plot",
      hline=NULL,add=FALSE)

##-
## Verificando valor de m Análise Fatorial(AF) e Explicações das variancias de cada fator

Var_expl = c(6.43, 2.25, 1.33, 0.94, 0.71, 0.55, 0.28, 0.19, 0.14, 0.09, 0.05, 0.04, 0) # Valores ss loadings
Perc_Var_expl = Var_expl/sum(Var_expl) # Valores Proportion Var
Perc_Acuml_Var_expl = cumsum(Perc_Var_expl) # Valores Cumulative Var

Tab_autov_mat_correl <- data.frame(Var_expl, Perc_Var_expl,Perc_Acuml_Var_expl)
names(Tab_autov_mat_correl) <- c("Explicação Variavel(Autovalores)", "% Explicação Variavel", "% Acumulada Explicação Variavel")

## Pelo critério de Kaiser, m deve ser igual a 3 poís é o numero de autovetores maiores que 1
## No entanto, para explicar um percentual >90%, m deve ser igual a 6

##-
## Gerando a solução inicial de AF com o número de fatores (m=3) de Kaiser, sem rotação

AF1.corr <- principal(FSAUDE[ ,-1], nfactors=3, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF1.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Interpretação dos fatores para m=3

##  PC1 está mais correlacionada com as variaveis PESO, CINTURA, IMC e BRACO. PC2 é mais correlacionada com PERNA e PC3 com TX_PULSACAO 

## As variáveis DIASTOLICA e PULSO tem graus de correlação parecidos para PC1 e PC2

## A variável ALTURA tem graus de correlação parecidos para PC2 e PC3


############################ Questão 1 Letra B ####################################
## Faça uma rotação ortogonal Varimax dos fatores encontrados em (a). 

##-
## Rotação Varimax

AF2.corr_rot <- principal(FSAUDE[ ,-1], nfactors=3, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF2.corr_rot, digits=4, cutoff=.0,sort=FALSE)



############################ Questão 1 Letra B1 ####################################
## Escreva a composição dos fatores (ou seja as variáveis alocadas em cada fator) e interprete 
## os fatores da  solução rotacionada. Qual solução (sem ou com rotação) em termos de interpretação 
## lhe parece melhor?  Justifique.  

##-
## RC1 continua mais correlacionada com as variaveis PESO, CINTURA, IMC e BRACO. Já RC2 é mais correlacionada com ALTURA e PERNA e RC3 com TX_PULSACAO  e DIASTOLICA

## Em termos de interpretação, me parece melhor a solução com rotação pelos motivos de não terem 
## variáveis com graus de covariancia parecidos entre os fatores e o percentual acumulado de 
## explicação ser semelhante entre as duas soluções



############################ Questão 1 Letra B2 ####################################
## Dê as comunalidades, variâncias específicas e complexidades estimadas (antes da rotação e 
## depois da rotação ortogonal).  Compare  esses valores com os obtidos  (a). Existe alguma 
## diferença?

##-
## A comunalidade(h2) e a variância específica(u2) são semelhantes para as duas soluções
## Já a complexidade é diferente sendo que, quanto mais próximo de 1 melhor é a solução da análise fatorial. Na rotação temos 3 variaveis com complexidade acima de 2, sem rotação temos 2 variaveis apenas;



############################ Questão 1 Letra B3 ####################################
## Considerando suas respostas em (b1) e (b2), qual solução lhe parece melhor? 
## (com ou sem rotação?). Justifique.

##-
## Me parece melhor a solução sem rotação, por ter complexidades mais próximas de 1 e por ter variaveis bem correlacionadas nos tres fatores



############################ Questão 1 Letra C ####################################
## Agora adicione um fator à sua solução encontrada em (a) (solução sem rotação ortogonal). 
## Faça a rotação varimax dessa solução. Responda o item (b) novamente considerando essa nova
## solução. Compare essa novasolução com as obtidas em (a) e (b).

##-
## Adicionando um fator

AF3.corr <- principal(FSAUDE[ ,-1], nfactors=4, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF3.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Realizando rotação varimax

AF4.corr_rot <- principal(FSAUDE[ ,-1], nfactors=4, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF4.corr_rot, digits=4, cutoff=.0, sort=FALSE)

##-
## Comparando resultados

## Para m=4, me parece melhor a solução com rotação, por ter complexidades mais próximas de 1 e por ter variaveis bem correlacionadas nos quatro fatores



############################ Questão 1 Letra D ####################################
## Agora adicione dois fatores à sua solução encontrada em (a) (solução sem rotação ortogonal). 
## Faça a rotação varimax dessa solução. Responda o item (b) novamente considerando essa nova 
## solução. Compare essa nova solução com as obtidas em (a),  (b) e (c).

##-
## Adicionando mais um fator

AF4.corr <- principal(FSAUDE[ ,-1], nfactors=5, 
                      n.obs=40,rotate='none', scores=FALSE)

print(AF4.corr, digits=4, cutoff=.0, sort=FALSE)

##-
## Realizando rotação varimax

AF5.corr_rot <- principal(FSAUDE[ ,-1], nfactors=5, 
                          n.obs=40, rotate='varimax', scores=TRUE)

print(AF5.corr_rot, digits=4, cutoff=.0, sort=FALSE)

##-
## Comparando resultados

## Para m=5, me parece melhor a solução com rotação, por ter complexidades mais próximas de 1 e por ter variaveis bem correlacionadas nos cinco  fatores



############################ Questão 1 Letra E ####################################
## Gere os escores dos fatores da solução que considerou a melhor (entre os itens (a), (b), (c), 
## (d)). Mostre os pesos de ponderação que são usados para os cálculos dos escores gerados. 
## Escreva a equação correspondente ao cálculo dos escores do primeiro fator.


##-
## Comando para visualizar os escores dos elementos amostrais (m=5) rotação varimax

print(AF5.corr_rot$scores, digits=4, cutoff=.0, sort=FALSE)



############################ Questão 1 Letra G ####################################
## Calcule os valores do índice KMO para cada uma das 13 variáveis envolvidas nessa análise 
## fatorial. Inteprete os resultados.


##-
## Comando para cálculo da medida descritiva KMO

KMO(r = FSAUDE [ , -1])

## KMO's próximos de 1 há boas chance de ter agrupamento entre as variaveis.