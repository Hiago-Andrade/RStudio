### IMPORTANTE! ########################################
###
### Para evitar "dores de cabeca" ao trabalhar com o R,
### seja via console, seja via outros programas como o 
### RStudio, crie uma PASTA DE TRABALHO e coloque lah
### todos os arquivos necessarios: scripts e arquivos 
### de dados.
### Ao abrir o script a partir dessa pasta, o R vai
### reconhecer essa pasta como seu diretorio de trabalho
########################################################

##################################################
## INSTRUÇOES INICIAIS (para usar o console do R)

## Abrir com qualquer leitor de texto ou dentro do R
##     no menu Arquivo > Abrir script
## Todos os arquivos de dados devem estar no diretorio de trabalho
## Tenha certeza de que você mudou o diretorio de trabalho, indicando ao R
##    onde estão seus arquivos de trabalho (scripts e dados).
##    Menu Arquivo > Mudar dir
## Use o comando getwd() para verificar se o diretório de trabalho está correto


#############################
## Lendo um arquivo de dados

#Lendo arquivo de texto separado por tabulacoes (.txt)
dados <- read.table("EST006_Bears_short.txt",
                    header=TRUE, dec=",", sep="\t")
str(dados)  #Mostra a estrutura dos dados


#Lendo arquivo de texto separado por virgula (ou ponto-e-virgula)
# (.csv - Comma Separated Values) 
dados <- read.csv("EST006_Bears_short.csv",
                    header=TRUE, dec=",", sep=";") #ponto-e-virgula como separador de campos
str(dados)  #Mostra a estrutura dos dados

## !Nao funciona em sistemas que usam virgula com separador de decimais!
dados <- read.csv("EST006_Bears_short.csv",
                  header=TRUE, dec=".", sep=",") #virgula como separador de campos
                                                 # e ponto como separador de decimais

#Lendo arquivo de planilha do Excel (.xlsx)
if(!require(readxl)){install.packages("readxl")
        require(readxl)} #carrega ou instala o pacote readxl 
dados <- read_excel("EST006_Bears_short.xlsx", sheet=1) 
str(dados)

dados <- data.frame(dados)     # Transformando o objeto dados em um data frame, 
                                   # o tipo de objeto que é solicitado pela maioria das funções do R.

nomes<-names(dados)       # retorna os nomes das colunas do objeto do tipo data.frame                                  
nomes

## Mudando os nomes das colunas
names(dados)[c(2,4)]<- c("Nome","Mes")
names(dados)

dados$Idade.meses              # Vendo os dados da variavel Idade
dados[,c("Idade.meses","Peso")]  # Vendo os dados das variaveis Idade e Peso


#######################################
## Lendo dados direto do teclado

dados <- scan()         # Leitura de dados numericos via teclado
#   Apertar a tecla ENTER a cada dado novo e ENTER duas vezes 
#   depois do ultimo dado inserido

#Escaneamento da entrada do teclado 
rural <- scan()     #Ex: quantidade de arsenio na água de 10 localidades    
48
44 
40 
38 
33 21 20 12 1 18
              #no console do R, apertar a tecla <enter>

rural

#Alternativa: usar funcao c() para construir um vetor

urbana <- c(3,7,25,10,15,
            6,12,25,15,7)    
urbana

### Descritivas
summary(rural)
summary(urbana)
