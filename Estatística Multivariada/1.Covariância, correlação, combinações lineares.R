############################################################################
## Disciplina Métodos de Estatística Multivariada 
## Especializacao em Estatistica - UFMG
## Covariância, correlação, combinações lineares
## Data: 29/01/2022
############################################################################

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

############################ Questão 1 Letra A ####################################
## Considere o exemplo 1 visto em sala de aula (slides). 
## banco de dados disponibilizado: notas_produto.txt
##
## Encontre as duas componentes principais usando os dados padronizados 
## (ou seja, faça a PCA via matriz de correlação. Lembrar que usando o pacote MVar.
## pt disponibilizado não é necessário padronizar os dados para obter as 
## componentes. Basta usar o comando PCA escolhendo a opção type=2). 
## Escreva as equações das duas componentes principais (Lembrar que neste exercício 
## as equações das componentes principais serão funções das variáveis padronizadas). 
## Qual seria o valor numérico dessas duas componentes se as notas atribuídas fossem 
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
## Matriz de Correlação
PC_correlacao <- PCA (notas_produto[,-1], type=2)

## $mtxc é a matriz de correlação R
## $mtxAutvlr autovalores e % de variância total explicado
## $mtxAutvec são os componentes (ler pelas colunas)
## $mtxCCP são as correlações das componentes com as variáveis
## $mtxscores são os escores das componentes
PC_correlacao

##--
## Equações das componentes principais

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


############################ Questão 1 Letra B ####################################

## Interprete as duas componentes principais, a correlação entre as componentes com 
## as duas variáveis (cor e sabor) e os escores das 2 componentes. Discuta os 
## resultados em termos da avaliação geral do produto. (Faça uma análise similar 
## ao que foi feito em sala de aula).
## As conclusões são similares àquelas obtidas na solução PCA via matriz de 
## covariâncias vista em sala de aula? Justifique.


##--
## Interpretação das componentes

## A PC1 é um índice geral de opinião do entrevistado sobre o produto, sendo que 
## valores altos indicam boa avaliação do produto. 

## PC2 é uma comparação ponderada da opinião do entrevistado 
## sobre a cor da embalagem com sua opinião em relação a sabor do 
## produto, devido o sinal invertido das componentes.

##--
## Interpretação da correlação entre as componentes

## PC1 é mais correlacionada com as duas variáveis (equivalente para as duas variaveis) 

## PC2 tem menor correlação que PC1, mas ainda assim há correlação consideravel.
## Sob o ponto de vista da análise de correlação com as variáveis 
## originais, a PC1 é a que melhor descreve (ou sintetiza) as 
## informações dos dados originais.

##--
## Interpretação dos escores das 2 componentes

## Pelos escores observados de PC1, tem-se que a primeira pessoa 
## avaliou melhor o produto, subsequente da segunda e da terceira. A terceira foi 
## a que pior avaliou o produto.

## --
## As conclusões são similares àquelas obtidas na solução PCA via matriz de 
## covariâncias vista em sala de aula? Justifique.

## As concusões são similares àquelas obtidas em aula por meio da matriz
## de covariância devido os dados estarem na mesma unidade de medida. Dessa forma,
## tanto a covariancia quanto a correlação podem ser utilizadas sem que haja o risco 
## do valor da covariancia ser afetado por alguma unidades de medida diferente.


############################ Questão 2 Letra A ####################################

## Considere os dados das 27 capitais brasileiras relativo ao Censo de 2010 (IBGE) 
## (arquivo  dados_capitais_censo_2010.txt).

## Considere as seguintes variáveis: X1: % de população residente com renda 
## domiciliar mensal per capita < 1/2 salário mínimo (sm= R$510,00 na época); 
## X2: Taxa de Analfabetismo (%); 
## X3: Taxa de Desemprego (%); 
## X4: % de população sem abastecimento de água (%); 
## X5: % de população sem esgoto (sem coleta de rede ou fossa séptica) ; 
## X6: % de população sem coleta de lixo.

## Faça a análise de componentes principais (PCA) via matriz de covariâncias 
## (de acordo com os procedimentos mostrados nos exemplos de sala de aula) 
## utilizando essas 6 variáveis. Interprete numericamente a 1ª. e a 2ª componentes principais.   

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

## $mtxc é a matriz de covariancia S
## $mtxAutvlr autovalores e % de variância total explicado
## $mtxAutvec são os componentes (ler pelas colunas)
## $mtxVCP são as covariância das componentes com as variáveis
## $mtxCCP são as correlações das componentes com as variáveis
## $mtxscores são os escores das componentes
PC_covariancia

##--
## Adicionando as componentes no data frame
censo_2010 = cbind(censo_2010_FULL, PC_covariancia$mtxscores)

##--
## Interpretação das componentes

## A PC1 é um índice geral das 6 variáveis analisadas, 

## PC2 é uma comparação ponderada das três primeiras variáveis
## sobre sobre as três últimas

##--
## Interpretação da correlação entre as componentes

## PC1 é mais correlacionada com as duas variáveis (mais relacionado com a variavel P_s_esgoto) 

## PC2 tem menor correlação que PC1, mas ainda assim há correlação consideravel (não possui quase correlação alguma com a variável P_s_esgoto).
## Sob o ponto de vista da análise de correlação com as variáveis 
## originais, a PC1 é a que melhor descreve (ou sintetiza) as 
## informações dos dados originais.


############################ Questão 2 Letra B ####################################

## Usando a primeira componente principal indique quais (em sua opinião) são as 5 melhores e quais são 
## as 5 piores capitais brasileiras de acordo com as 4 variáveis avaliadas. Justifique claramente sua 
## solução.

##--
## Descobrindo o escore otimo
PC1_otimo = sum((PC_covariancia$mtxAutvec[,1]) * (c(min(censo_2010_FULL$Renda_m_1_2_sm),min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo))))
PC1_otimo

PC2_otimo = sum((PC_covariancia$mtxAutvec[,2]) * (c(min(censo_2010_FULL$Renda_m_1_2_sm),min(censo_2010_FULL$Tx_An),min(censo_2010_FULL$Tx_dese),min(censo_2010_FULL$P_s_agua),min(censo_2010_FULL$P_s_esgoto),min(censo_2010_FULL$P_s_lixo))))
PC2_otimo

##--
## Interpretação dos escores das 2 componentes

## Pelos escores observados de PC1, tem-se que as 5 melhores capitais são: CURITIBA, VITÓRIA, BELO HORIZONTE, PORTO ALEGRE E RIO DE JANEIRO
## Já as 5 piores seriam: MACAPÁ, PORTO VELHO, MACIÓ, RIO BRANCO e BOA VISTA
## Essa classificação foi realizada considerando o valor otimo de PC1. As capitais com PC1 mais proximas do valor ótimo foram as melhores classificadas.


############################ Questão 2 Letra C ####################################

## Repita o item (a) fazendo a análise de componentes principais via matriz de correlação. 
## Compare essa solução com a realizada em sala de aula na qual a variável X7: Renda média 
## mensal domiciliar per capita (em reais), foi utilizada (ao invés da variável X1: % de população 
## residente com renda domiciliar mensal per capita < 1/2 salário mínimo).  Os resultados 
## das duas análises indicam a mesma conclusão?

##--
## Matriz de correlação
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
## O resultado das duas análises não indica a mesma conclusão. São diferentes as 5 melhores e piores capitais.



############################ Questão 2 Letra D ####################################

## Para esse problema cujo objetivo é identificar as capitais com melhor e pior condição 
## social, qual das três análises de componentes principais você recomendaria? 
## Jusfique sua resposta. 

##--
## Eu recomendaria a análise da componente principal PC1 por meio do PCA da matriz de correlação.
## Visto que ela apresentou elevada correlação entre as variaveis e considera a variavel renda per capta.
## No entanto, daria continuidade criando um índice de qualidade único usando as componentes principais PC1 e PC2 do PCA da matriz de correlação para elevar o % de variância explicada


############################ Questão 3######## ####################################

## Apanhe os dados do exemplo 1 visto nos slides em sala de aula (notas_produto.txt). 
## Padronize (subtração da média e divisão pelo desvio padrão) as duas variáveis 
## (opinião sobre cor da embalagem e sabor do produto). Calcule a covariância amostral 
## entre as duas variáveis padronizadas. Compare os valores obtidos com os valores da 
## matriz de correlação das duas variáveis  originais mostrados nos slides em sala de aula. 
## O que você pode concluir?

#--
#Limpando a memoria do R:
rm(list=ls(all=TRUE))

##--
#Lendo arquivo de texto separado por tabulacoes (.txt)
notas_produto <- read.table("notas_produto.txt",
                            header=TRUE)
str(notas_produto)

##--
## Padronização

Padronizacao_cor = c(((notas_produto$Cor - mean(notas_produto$Cor)) / sd(notas_produto$Cor) ))
Padronizacao_sabor = c(((notas_produto$Sabor - mean(notas_produto$Sabor)) / sd(notas_produto$Sabor)))

##--
## Juntando as variaveis padronizadas no data frame
notas_produto_padronzado = cbind(notas_produto, Padronizacao_cor, Padronizacao_sabor)

##--
## Calculo da Covariancia
cov(notas_produto_padronzado$Padronizacao_cor, notas_produto_padronzado$Padronizacao_sabor)


##--
## Comparação entre os valores obtidos com os valores da matriz de correlação das duas variáveis  
## originais mostrados nos slides em sala de aula

## Valor Obtido = 0.3273
## Valor aula correlação = 0.327

## A covariancia das variaveis padronizadas e a correlação das variaveis originais deram o
## mesmo valor. Conclui-se que a padronização colocou as variáveis na mesma escala, ideial
## para realizar comparações de variaveis diferentes.