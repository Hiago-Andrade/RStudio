######################################################################
## Hiago Andrade Duarte
######################################################################



rm (list=ls())
#
# Ajuste do Modelo Logístico - Análise Discriminante
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
# Definindo "SOC" como a especialidade de referência 
# 
is.factor(dados$Especialidade)
#
dados$fespec = factor(dados$Especialidade, c("SOC", "SCI", "BUS", "HUM"))
#
levels(dados$fespec)
#
# Ajustando o modelo logístico com todas as variáveis
#
glm.fit <- glm(risco ~ genero + idade + rendimento_academico + hrs + fespec, data= dados,family = binomial)
#
summary(glm.fit)
#
# armazenando os resultados das probabilidades estimadas
#
glm.probs <- predict(glm.fit,type = "response")
#
# Apresentando algumas predições em 3 cadas decimais
#
round(head(glm.probs),3)
#  
# Fazendo a classificacao: ponto de referência=0.5
# prob > 0.5 classificação no grupo 1
#
glm.pred <- ifelse(glm.probs > 0.5, "1", "0")
#
head(glm.pred,5)
#
# Fazendo tabelas para ver resultados (proporções de acerto e erro)
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
# Organizando uma tabela com proporção de erros e acertos
#
tab1<- rbind(cbind(tab,prop_acerto),c(prop_erro, prop_acerto_total))
#
tab1
#
#------------------------------------------------------------
# Ajuste do 2o. modelo (só com variáveis significativas)
# Cálculo de Intervalo de Confiança para os coeficentes
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
# Construindo as tabelas e cálculos e proporções de erros e acertos
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
# Organizando uma tabela proporções de erros e acertos
#
tab3<- rbind(cbind(tab2,prop_acerto),c(prop_erro,prop_acerto_total))
#
tab3
#
# Cálculo de Razão das Chances (Odds Ratio)
#
OR <- exp(glm.fit1$coefficients)
#
ORround<- round(OR,4)
#
# Cálculo de Intervalo de Confiança para OR
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
# Definindo a categoria de referência para os novos dados. 
# a categoria precisa ser a mesma usada no ajuste do modelo que vai ser usado para fazer as classificações #
#
dados_novos$fespec = factor(dados_novos$Especialidade, c("SOC", "SCI", "BUS", "HUM"))
#
# Comando para fazer a estimação da probabilidade de pertinência dos novos dados
#
# Nesse exemplo a predição é feita com a solução do modelo logístico 2 visto em sala de aula # 
#
pred_novos <- predict(glm.fit1, newdata = dados_novos)
#
#
# Comando para fazer as classificações dos novos elementos
#
pred_novos_classificacao <- ifelse(pred_novos > 0.5,"1", "0")
#
pred_novos_classificacao
#
# Comando para gerar as probabilidades de pertinência usadas na classificação dos novos elementos
#
prob_novos_1<- (exp(pred_novos))/ (1 + exp(pred_novos))
#
# arquivo contendo os dados novos e respectivas classificações (modelo logístico 2) #
#
dados_novos_class<- data.frame(dados_novos, prob_novos_1,pred_novos_classificacao)
#
head(dados_novos_class)
#
# arquivo contendo os dados originais e respectivas classificações (modelo logístico 2) #
#
dados_originais_class<- data.frame(dados,glm.pred, glm.probs1)
#
head(dados_originais_class,5)
#