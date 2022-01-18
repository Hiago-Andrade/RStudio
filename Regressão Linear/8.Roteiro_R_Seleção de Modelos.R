############################################################################

## Objetivo: Selecao de modelos.
############################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))


#-- Sobre o conjunto de dados:
### Vamos usar a base de dados EURO4PlayerSkillsSep11 do pacote SportsAnalytics.
### O objetivo E' modelar a precisao em passes de longa distancia de jogadores
### de futebol funcao de covariaveis referentes a caracteristicas dos jogadores.

if(!require(SportsAnalytics)){install.packages("SportsAnalytics"); require(SportsAnalytics)} 
if(!require(car)){install.packages("car"); require(car)} # Instala pacote car

data(EURO4PlayerSkillsSep11) #colocando no ambiente do R
help(EURO4PlayerSkillsSep11)
str(EURO4PlayerSkillsSep11)

### Selecionando as covari???veis para an???lise.
dados <- EURO4PlayerSkillsSep11[-1166,c('Agility' , 'Acceleration' , 'TopSpeed', 
                                        'Balance', 'Jump', 'Teamwork', 'Mentality', 
                                        'ConditionFitness','Height', 'Age', 'Weight',
                                        'LongPassAccuracy')]
dados <- na.omit(dados) #Tirando os dados vazios

### O jogador da linha 1166 foi excluido pois os dados correspondentes, 
### em sua maioria, eram iguais a zero (provavelmente dados missing). Alem
### disso, 37 jogadores foram excluidos da base por nao terem todas
### as informacoes disponiveis. A base final para analise tem 1813 jogadores.

### Antes de usar a fun??????o 'step' para selecionar covariaveis via testes
### de hipoteses usando os algoritmos backward, forward e stepwise,
### vamos fazer isso 'no braco'.

########################################################################
### Algoritmo backward

ajustek <- lm(LongPassAccuracy ~ ., data = dados) #Dessa forma, o R entende que todas as colunas do arquivo são variáveis explicativas

### Ajuste com todas as covariaveis.

drop1(ajustek, test = 'F')
### A funcao drop1 apresenta os resultados do teste F produzidos mediante
### extracao das covariaveis do modelo (uma a uma - 'backward'). A variavel 
### 'Age' tem maior p-valor (p = 0.9698) e sera eliminada do modelo.

ajuste2 <- update(ajustek, ~.-Age) ### Modelo ajustado sem a vari???vel Age.
drop1(ajuste2, test = 'F')
### A vari???vel ConditionFitness tem maior p-valor (p = 0.6295) e ser??? 
### eliminada do modelo.

### Segue o processo.

ajuste3 <- update(ajuste2, ~.-ConditionFitness)
drop1(ajuste3, test = 'F') ### Sai Acceleration.

ajuste4 <- update(ajuste3, ~.-Acceleration)
drop1(ajuste4, test = 'F') ### Sai Mentality.

ajuste5 <- update(ajuste4, ~.-Mentality)
drop1(ajuste5, test = 'F') ### Sai Weight.

ajuste6 <- update(ajuste5, ~.-Weight)
drop1(ajuste6, test = 'F')
### Todas as vari???veis remanescentes t???m efeito significativo. O processo
### ??? encerrado com o modelo atual, sem excluir novas covari???veis.

summary(ajuste6) ### Resumo do modelo final.
vif(ajuste6)     ### Calcula o VIF, pacote 'car'

########################################################################
### Algoritmo forward

saida0 <- lm(LongPassAccuracy ~ 1, data = dados)
### Modelo nulo (s??? com intercepto).

add1(saida0, scope=~Agility + Acceleration + TopSpeed 
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F')
### A funca add1 apresenta os resultados do teste F produzidos mediante
### inclusao das covariaveis do modelo (uma a uma - 'forward').
### Termo 'scope' deve conter os termos a serem adicionados ou removidos.
### A variavel TopSpeed tem maior valor para a estatistica F (F = 865.2358), 
### e p-valor extremamente baixo e sera incluida ao modelo.

### Nota - neste caso avaliar menor p-valor e maior valor da estatistica F
### e' equivalente apenas porque todas as covariaveis tem um grau de liberdade igual.

saida1 <- lm(LongPassAccuracy ~ TopSpeed, data = dados)
### Modelo com a inclusao de TopSpeed.

add1(saida1, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F')
### A inclusao da variavel Teamwork produziu maior valor para a estatistica
### F (e p-valor extremamente baixo) e sera incluida ao modelo.

### Segue o processo.

saida2 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork, data = dados)

add1(saida2, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Jump.

saida3 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + Jump, data = dados)

add1(saida3, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Balance.

saida4 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + Jump + Balance, data = dados)
add1(saida4, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Agility.

saida5 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + 
               Jump + Balance + Agility, data = dados)
add1(saida5, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Height.

saida6 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + 
               Jump + Balance + Agility + Height, data = dados)
add1(saida6, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') 

### As variaveis que ainda nao foram incluidas no modelo nao apresentam
### efeito significativo, segundo o teste F. O processo e' encerrado
### com o modelo atual, sem adicionar novas covariaveis.

########################################################################
### Agora usando a funcao 'step' do R.

### Selecao backward.
mod_back <- step(ajustek, direction = 'backward', test = 'F')

### Sele??????o forward
mod_for <- step(saida0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', test = 'F')

### Sele??????o stepwise.
mod_step <- step(ajustek, direction = 'both', test = 'F')

compareCoefs(mod_back, mod_for, mod_step) #pacote 'car'

### Os tres metodos produziram o mesmo modelo (mesmo conjunto de covariaveis
### selecionadas). Isso nao acontece sempre, e' plenamente possivel obter
### modelos diferentes usando metodos de selecao diferentes.



########################################################################
### An???lise baseada em crit???rios de ajuste.

if(!require(leaps)){install.packages("leaps"); require(leaps)} # 
help("regsubsets")

all_reg <- regsubsets(LongPassAccuracy ~ Agility + Acceleration  + TopSpeed
                      + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                      + Height + Age + Weight, method = "exhaustive",
                      nvmax = 11, data = dados)

### A funcao regsubsets vai ajustar todos os modelos de regressao possiveis,
### e armazenar os valores dos criterios de qualidade para os melhores ajustes
### com j = 1, j = 2, ..., j = k covariaveis.

plot(all_reg)
### BICs para os modelos otimos para cada numero de covariaveis.

s1 <- summary(all_reg, matrix.logical=TRUE)
s1

### Cada linha da matriz logica apresenta o melhor modelo para um particular
### numero de covariaveis. Neste caso TRUE indica que a variavel e' incluida
### no modelo e FALSE que ela nao e' incluida.

### Assim, a titulo de exemplo, o melhor modelo com uma covariavel tem 
### "TopSpeed" como regressora; o melhor modelo com duas covariaveis e'
### ajustado por "TopSpeed" e "Teamwork" e assim por diante.

### Como nesse primeiro momento apenas sao comparados modelos com igual
### numero de parametros, qualquer criterio de qualidade de ajuste vai 
### indicar a selecao do mesmo modelo.

### Para comparar a sequencia de modelos obtidos para diferentes numeros de
### covariaveis, podemos recorrer aos criterios de qualidade de ajuste 
### estudados.

s1$rsq ### R2
s1$adjr2 ### R2 ajustado
s1$cp ### Cp de Mallows
s1$bic ### BIC

which.max(s1$adjr2) 
### O modelo com seis covariaveis produziu maior valor de R2 ajustado.

which.min(s1$bic) 
### O modelo com seis covariaveis produziu menor valor de BIC.

which.max(s1$rsq) 
### O modelo com onze covariaveis produziu menor valor de R2 (obviamente).

### Vamos produzir graficos.
n_cov <- 1:11

plot(n_cov, s1$rsq, type = 'b', xlab = 'N???mero de covari???veis', ylab = 'R2', las = 1, pch = 20)
plot(n_cov, s1$adjr2, type = 'b', xlab = 'N???mero de covari???veis', ylab = 'Adjusted R2', las = 1, pch = 20)
plot(n_cov, s1$bic, type = 'b', xlab = 'N???mero de covari???veis', ylab = 'BIC', las = 1, pch = 20)


### Agora a analise do Cp de Mallows.
plot(n_cov, s1$cp, xlab = 'N???mero de covari???veis', ylab = 'Cp de Mallows', las = 1, pch = 20)
abline(0,1)
### Os elevados valores de Cp para modelos com uma at??? tres covariaveis 
### dificulta a visualizacao dos resultados.

plot(n_cov[3:11], s1$cp[3:11], xlab = 'N???mero de covari???veis', ylab = 'Cp', las = 1, pch = 20)
abline(0,1)
### O modelo com menor Cp proximo 'a reta identidade e' o modelo com seis
### covariaveis.

########################################################################

### Agora vamos repetir a an???lise usando os algoritmos backward, forward e 
### stepwise com base na minimiza??????o dos crit???rios AIC e BIC. Primeiro
### usando AIC (k=2 ??? a constante de penaliza??????o).

### Sele??????o backward, crit???rio AIC.
mod_back <- step(ajustek, direction = 'backward', k = 2)
summary(mod_back)

### Sele??????o forward, crit???rio AIC.
mod_for <- step(saida0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', k = 2)
summary(mod_for)


### Sele??????o stepwise, crit???rio AIC.
mod_step <- step(ajuste, direction = 'both', k = 2)
summary(mod_step)

### usando BIC (k=log(n) ??? a constante de penaliza??????o).

### Sele??????o backward, crit???rio BIC.
n <- nrow(dados)
mod_back <- step(ajustek, direction = 'backward', k = log(n))
summary(mod_back)

### Sele??????o forward, crit???rio BIC.
mod_for <- step(saida0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', k = log(n))
summary(mod_for)

### Sele??????o stepwise, crit???rio BIC.
mod_step <- step(ajustek, direction = 'both', k = log(n))
summary(mod_step)


#--
#
print("Fim do script: Laboratorio 4, Parte B!")