############################################################################
## Disciplina M�todos de Estat�stica Multivariada 
## Especializacao em Estatistica - UFMG
## Covari�ncia, correla��o, combina��es lineares
## Data: 29/01/2022
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Quest�o 1 Letra A ####################################
## Considere o exemplo 1 visto em sala de aula (slides). 
## banco de dados disponibilizado: notas_produto.txt
##
## Encontre as duas componentes principais usando os dados padronizados 
## (ou seja, fa�a a PCA via matriz de correla��o. Lembrar que usando o pacote MVar.
## pt disponibilizado n�o � necess�rio padronizar os dados para obter as 
## componentes. Basta usar o comando PCA escolhendo a op��o type=2). 
## Escreva as equa��es das duas componentes principais (Lembrar que neste exerc�cio 
## as equa��es das componentes principais ser�o fun��es das vari�veis padronizadas). 
## Qual seria o valor num�rico dessas duas componentes se as notas atribu�das fossem 
## iguais a 10 para Cor e Sabor? 

##-- 
## Ler conjunto de dados:
notas_produto <- read.table("notas_produto.txt",
                            header=TRUE)
str(notas_produto)

##-- 
## Instalando o pacote:
if(!require(MVar.pt)){install.packages("MVar.pt")
  require(MVar.pt)} 

##--
## Matriz de Correla��o
PC_correlacao <- PCA (notas_produto[,-1], type=2)

## $mtxc � a matriz de correla��o R
## $mtxAutvlr autovalores e % de vari�ncia total explicado
## $mtxAutvec s�o os componentes (ler pelas colunas)
## $mtxCCP s�o as correla��es das componentes com as vari�veis
## $mtxscores s�o os escores das componentes
PC_correlacao

##--
## Equa��es das componentes principais

PC1 = c(-0.7071068 * ((notas_produto$Cor - mean(notas_produto$Cor)) / sd(notas_produto$Cor) )) + (-0.7071068 * ((notas_produto$Sabor - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))
PC1

PC2 = c(0.7071068 * ((notas_produto$Cor - mean(notas_produto$Cor)) / sd(notas_produto$Cor) )) + (-0.7071068 * ((notas_produto$Sabor - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))
PC2

##--
## Adicionando PC1 e PC2 no data frame
notas_produto = cbind(notas_produto, PC1, PC2)

##--
## Resultado de PC1 e PC2 se as notas forem 10 para cor e sabor

PC1_otimo = (-0.7071068 * ((10 - mean(notas_produto$Cor)) / sd(notas_produto$Cor) )) + (-0.7071068 * ((10 - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))
PC1_otimo

PC2_otimo = (0.7071068 * ((10 - mean(notas_produto$Cor)) / sd(notas_produto$Cor) )) + (-0.7071068 * ((10 - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))
PC2_otimo


############################ Quest�o 1 Letra B ####################################

## Interprete as duas componentes principais, a correla��o entre as componentes com 
## as duas vari�veis (cor e sabor) e os escores das 2 componentes. Discuta os 
## resultados em termos da avalia��o geral do produto. (Fa�a uma an�lise similar 
## ao que foi feito em sala de aula).
## As conclus�es s�o similares �quelas obtidas na solu��o PCA via matriz de 
## covari�ncias vista em sala de aula? Justifique.


##--
## Interpreta��o das componentes

## A PC1 � um �ndice geral de opini�o do entrevistado sobre o produto, sendo que 
## valores altos indicam boa avalia��o do produto. 

## PC2 � uma compara��o ponderada da opini�o do entrevistado 
## sobre a cor da embalagem com sua opini�o em rela��o a sabor do 
## produto, devido o sinal invertido das componentes.

##--
## Interpreta��o da correla��o entre as componentes

## PC1 � mais correlacionada com as duas vari�veis (equivalente para as duas variaveis) 

## PC2 tem menor correla��o que PC1, mas ainda assim h� correla��o consideravel.
## Sob o ponto de vista da an�lise de correla��o com as vari�veis 
## originais, a PC1 � a que melhor descreve (ou sintetiza) as 
## informa��es dos dados originais.

##--
## Interpreta��o dos escores das 2 componentes

## Pelos escores observados de PC1, tem-se que a primeira pessoa 
## avaliou melhor o produto, subsequente da segunda e da terceira. A terceira foi 
## a que pior avaliou o produto.

## --
## As conclus�es s�o similares �quelas obtidas na solu��o PCA via matriz de 
## covari�ncias vista em sala de aula? Justifique.

## As concus�es s�o similares �quelas obtidas em aula por meio da matriz
## de covari�ncia devido os dados estarem na mesma unidade de medida. Dessa forma,
## tanto a covariancia quanto a correla��o podem ser utilizadas sem que haja o risco 
## do valor da covariancia ser afetado por alguma unidades de medida diferente.


############################ Quest�o 2 Letra A ####################################

## Considere os dados das 27 capitais brasileiras relativo ao Censo de 2010 (IBGE) 
## (arquivo  dados_capitais_censo_2010.txt).

## Considere as seguintes vari�veis: X1: % de popula��o residente com renda 
## domiciliar mensal per capita < 1/2 sal�rio m�nimo (sm= R$510,00 na �poca); 
## X2: Taxa de Analfabetismo (%); 
## X3: Taxa de Desemprego (%); 
## X4: % de popula��o sem abastecimento de �gua (%); 
## X5: % de popula��o sem esgoto (sem coleta de rede ou fossa s�ptica) ; 
## X6: % de popula��o sem coleta de lixo.

## Fa�a a an�lise de componentes principais (PCA) via matriz de covari�ncias 
## (de acordo com os procedimentos mostrados nos exemplos de sala de aula) 
## utilizando essas 6 vari�veis. Interprete numericamente a 1�. e a 2� componentes principais.   

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

##--
#Lendo arquivo de texto separado por tabulacoes (.txt)
censo_2010_FULL <- read.table("dados_capitais_censo_2010.txt",
                              header=TRUE)
str(censo_2010_FULL)

##-- 
## Instalando o pacote:
if(!require(MVar.pt)){install.packages("MVar.pt")
  require(MVar.pt)} 

##--
## Matriz de covariancia
PC_covariancia <- PCA ( censo_2010_FULL[,c(4,5,6,7,8,9)], type=1)

## $mtxc � a matriz de covariancia S
## $mtxAutvlr autovalores e % de vari�ncia total explicado
## $mtxAutvec s�o os componentes (ler pelas colunas)
## $mtxVCP s�o as covari�ncia das componentes com as vari�veis
## $mtxCCP s�o as correla��es das componentes com as vari�veis
## $mtxscores s�o os escores das componentes
PC_covariancia

##--
## Adicionando as componentes no data frame
censo_2010 = cbind(censo_2010_FULL, PC_covariancia$mtxscores)

##--
## Interpreta��o das componentes

## A PC1 � um �ndice geral das 6 vari�veis analisadas, 

## PC2 � uma compara��o ponderada das tr�s primeiras vari�veis
## sobre sobre as tr�s �ltimas

##--
## Interpreta��o da correla��o entre as componentes

## PC1 � mais correlacionada com as duas vari�veis (mais relacionado com a variavel P_s_esgoto) 

## PC2 tem menor correla��o que PC1, mas ainda assim h� correla��o consideravel (n�o possui quase correla��o alguma com a vari�vel P_s_esgoto).
## Sob o ponto de vista da an�lise de correla��o com as vari�veis 
## originais, a PC1 � a que melhor descreve (ou sintetiza) as 
## informa��es dos dados originais.


############################ Quest�o 2 Letra B ####################################

## Usando a primeira componente principal indique quais (em sua opini�o) s�o as 5 melhores e quais s�o 
## as 5 piores capitais brasileiras de acordo com as 4 vari�veis avaliadas. Justifique claramente sua 
## solu��o.

##--
## Descobrindo o escore otimo
PC1_otimo = sum((PC_covariancia$mtxAutvec[,1]) * (c(min(censo_2010_FULL$Renda_m_1_2_sm),min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo))))
PC1_otimo

PC2_otimo = sum((PC_covariancia$mtxAutvec[,2]) * (c(min(censo_2010_FULL$Renda_m_1_2_sm),min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo))))
PC2_otimo

##--
## Interpreta��o dos escores das 2 componentes

## Pelos escores observados de PC1, tem-se que as 5 melhores capitais s�o: CURITIBA, VIT�RIA, BELO HORIZONTE, PORTO ALEGRE E RIO DE JANEIRO
## J� as 5 piores seriam: MACAP�, PORTO VELHO, MACI�, RIO BRANCO e BOA VISTA
## Essa classifica��o foi realizada considerando o valor otimo de PC1. As capitais com PC1 mais proximas do valor �timo foram as melhores classificadas.


############################ Quest�o 2 Letra C ####################################

## Repita o item (a) fazendo a an�lise de componentes principais via matriz de correla��o. 
## Compare essa solu��o com a realizada em sala de aula na qual a vari�vel X7: Renda m�dia 
## mensal domiciliar per capita (em reais), foi utilizada (ao inv�s da vari�vel X1: % de popula��o 
## residente com renda domiciliar mensal per capita < 1/2 sal�rio m�nimo).  Os resultados 
## das duas an�lises indicam a mesma conclus�o?

##--
## Matriz de correla��o
PC_correlacao <- PCA ( censo_2010_FULL[,c(5,6,7,8,9,10)], type=2)
PC_correlacao

##--
## Adicionando as componentes no data frame
censo_2010_correlacao = cbind(censo_2010_FULL, PC_correlacao$mtxscores)

##--
## Descobrindo o escore otimo
PC1_otimo_correlacao = sum((PC_correlacao$mtxAutvec[,1]) * (c(min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo),max(censo_2010_FULL$Renda_p_cap))))
PC1_otimo_correlacao

PC2_otimo_correlacao = sum((PC_correlacao$mtxAutvec[,2]) * (c(min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo),max(censo_2010_FULL$Renda_p_cap))))
PC2_otimo_correlacao

##--
## O resultado das duas an�lises n�o indica a mesma conclus�o. S�o diferentes as 5 melhores e piores capitais.



############################ Quest�o 2 Letra D ####################################

## Para esse problema cujo objetivo � identificar as capitais com melhor e pior condi��o 
## social, qual das tr�s an�lises de componentes principais voc� recomendaria? 
## Jusfique sua resposta. 

##--
## Eu recomendaria a an�lise da componente principal PC1 por meio do PCA da matriz de correla��o.
## Visto que ela apresentou elevada correla��o entre as variaveis e considera a variavel renda per capta.
## No entanto, daria continuidade criando um �ndice de qualidade �nico usando as componentes principais PC1 e PC2 do PCA da matriz de correla��o para elevar o % de vari�ncia explicada


############################ Quest�o 3######## ####################################

## Apanhe os dados do exemplo 1 visto nos slides em sala de aula (notas_produto.txt). 
## Padronize (subtra��o da m�dia e divis�o pelo desvio padr�o) as duas vari�veis 
## (opini�o sobre cor da embalagem e sabor do produto). Calcule a covari�ncia amostral 
## entre as duas vari�veis padronizadas. Compare os valores obtidos com os valores da 
## matriz de correla��o das duas vari�veis  originais mostrados nos slides em sala de aula. 
## O que voc� pode concluir?

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

##--
#Lendo arquivo de texto separado por tabulacoes (.txt)
notas_produto <- read.table("notas_produto.txt",
                            header=TRUE)
str(notas_produto)

##--
## Padroniza��o

Padronizacao_cor = c(((notas_produto$Cor - mean(notas_produto$Cor)) / sd(notas_produto$Cor) ))
Padronizacao_sabor = c(((notas_produto$Sabor - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))

##--
## Juntando as variaveis padronizadas no data frame
notas_produto_padronzado = cbind(notas_produto, Padronizacao_cor, Padronizacao_sabor)

##--
## Calculo da Covariancia
cov(notas_produto_padronzado$Padronizacao_cor, notas_produto_padronzado$Padronizacao_sabor)


##--
## Compara��o entre os valores obtidos com os valores da matriz de correla��o das duas vari�veis  
## originais mostrados nos slides em sala de aula

## Valor Obtido = 0.3273
## Valor aula correla��o = 0.327

## A covariancia das variaveis padronizadas e a correla��o das variaveis originais deram o
## mesmo valor. Conclui-se que a padroniza��o colocou as vari�veis na mesma escala, ideial
## para realizar compara��es de variaveis diferentes.