######################################################################
## Hiago Andrade Duarte
######################################################################



rm (list=ls())
#
# Ajuste do Modelo Log�stico - An�lise Discriminante
#
# Exemplo visto em sala de aula
#
dados<- read.table ("c:/dados/estudantes.txt", h=T)
#
head(dados,4)
#
#
attach(dados)
#
# Definindo "SOC" como a especialidade de refer�ncia 
# 
is.factor(dados$Especialidade)
#
dados$fespec = factor(dados$Especialidade, c("SOC", "SCI", "BUS", "HUM"))
#
levels(dados$fespec)
#
# Ajustando o modelo log�stico com todas as vari�veis
#
glm.fit <- glm(risco ~ genero + idade + rendimento_academico + hrs + fespec, data= dados,family = binomial)
#
summary(glm.fit)
#
# armazenando os resultados das probabilidades estimadas
#
glm.probs <- predict(glm.fit,type = "response")
#
# Apresentando algumas predi��es em 3 cadas decimais
#
round(head(glm.probs),3)
#  
# Fazendo a classificacao: ponto de refer�ncia=0.5
# prob > 0.5 classifica��o no grupo 1
#
glm.pred <- ifelse(glm.probs > 0.5, "1", "0")
#
head(glm.pred,5)
#
# Fazendo tabelas para ver resultados (propor��es de acerto e erro)
#
tab<- table(risco,glm.pred)
#
# Adicionando as marginais a tabela (soma das linhas e colunas)
#
addmargins(tab)
#
prop_acerto<- diag(prop.table(tab, 1))
#
prop_acerto
#
prop_erro <- 1 - prop_acerto 
#
prop_erro
#
prop_acerto_total<- sum(diag(prop.table(tab)))
#
prop_acerto_total
#
prop_erro_total <- 1 - prop_acerto_total
#
prop_erro_total
#
# Organizando uma tabela com propor��o de erros e acertos
#
tab1<- rbind(cbind(tab,prop_acerto),c(prop_erro, prop_acerto_total))
#
tab1
#
#------------------------------------------------------------
# Ajuste do 2o. modelo (s� com vari�veis significativas)
# C�lculo de Intervalo de Confian�a para os coeficentes
#
glm.fit1 <- glm(risco ~ rendimento_academico + hrs + fespec, data= dados,family = binomial)
#
summary(glm.fit1)
#
ICbeta=confint.default(glm.fit1,level=0.95)
#
# Agregando tudo numa tabela
#
tabcoef <- cbind(glm.fit1$coefficients,ICbeta)
#
tabcoef
#
glm.probs1 <- predict(glm.fit1,type = "response")
#
round(head(glm.probs1),3)
#
glm.pred1 <- ifelse(glm.probs1 > 0.5, "1", "0")
#
head(glm.pred1,5)
#
# Construindo as tabelas e c�lculos e propor��es de erros e acertos
#
tab2<- table(risco,glm.pred1)
#
# Adicionando as marginais a tabela (soma das linhas e colunas)
#
addmargins(tab2)
#
prop_acerto<- diag(prop.table(tab2, 1))
#
prop_acerto
#
prop_erro <- 1 - prop_acerto 
#
prop_erro
#
prop_acerto_total<- sum(diag(prop.table(tab2)))
#
prop_erro_total <- 1 - prop_acerto_total
#
prop_acerto_total
#
prop_erro_total
#
#
# Organizando uma tabela propor��es de erros e acertos
#
tab3<- rbind(cbind(tab2,prop_acerto),c(prop_erro,prop_acerto_total))
#
tab3
#
# C�lculo de Raz�o das Chances (Odds Ratio)
#
OR <- exp(glm.fit1$coefficients)
#
ORround<- round(OR,4)
#
# C�lculo de Intervalo de Confian�a para OR
#
ORIC = exp(ICbeta)
#
ORICround <-round(ORIC,4)
#
# Agregando os resultados numa tabela
#
tabOR <- cbind(ORround,ORICround)
#
hist(glm.probs1)
#
#--------------------------------------------------
#
# Classificacao de novos registros
#
# Leitura dos novos dados
#
dados_novos<- read.table ("c:/dados/estudantes_dados_novos.txt", h=T)
#
dados_novos
#
# Definindo a categoria de refer�ncia para os novos dados. 
# a categoria precisa ser a mesma usada no ajuste do modelo que vai ser usado para fazer as classifica��es #
#
dados_novos$fespec = factor(dados_novos$Especialidade, c("SOC", "SCI", "BUS", "HUM"))
#
# Comando para fazer a estima��o da probabilidade de pertin�ncia dos novos dados
#
# Nesse exemplo a predi��o � feita com a solu��o do modelo log�stico 2 visto em sala de aula # 
#
pred_novos <- predict(glm.fit1, newdata = dados_novos)
#
#
# Comando para fazer as classifica��es dos novos elementos
#
pred_novos_classificacao <- ifelse(pred_novos > 0.5,"1", "0")
#
pred_novos_classificacao
#
# Comando para gerar as probabilidades de pertin�ncia usadas na classifica��o dos novos elementos
#
prob_novos_1<- (exp(pred_novos))/ (1 + exp(pred_novos))
#
# arquivo contendo os dados novos e respectivas classifica��es (modelo log�stico 2) #
#
dados_novos_class<- data.frame(dados_novos, prob_novos_1,pred_novos_classificacao)
#
head(dados_novos_class)
#
# arquivo contendo os dados originais e respectivas classifica��es (modelo log�stico 2) #
#
dados_originais_class<- data.frame(dados,glm.pred, glm.probs1)
#
head(dados_originais_class,5)
#