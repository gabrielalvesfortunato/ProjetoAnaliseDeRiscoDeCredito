setwd("C:/Users/Gabriel/Desktop/Cursos/BigDataReAzure/MiniProjeto-AnaliseDeRiscoDeCredito")
getwd()

####### PROJETO DE ANALISE DE RISCO DE CREDITO #########

# Carregando pacotes
library(ggplot2)
library(readr)

# Obtendo os dados
dadosCredito <- read_csv("credit_dataset.csv")
View(dadosCredito)
str(dadosCredito)


# Convertendo as variaveis para fatores
toFactors <- function(dataframe, variaveis) {
  for(variavel in variaveis) {
    dataframe[[variavel]] <- as.factor(dataframe[[variavel]])
  }
  return(dataframe)
}

variaveisFatores <- c("credit.rating", "account.balance", "previous.credit.payment.status",
                      "credit.purpose", "savings", "employment.duration", "installment.rate",
                      "marital.status", "guarantor", "residence.duration", "current.assets",
                      "other.credits", "apartment.type", "bank.credits", "occupation", "dependents",
                      "telephone", "foreign.worker")

dadosCredito <- toFactors(dadosCredito, variaveisFatores)
str(dadosCredito)



#### ANALISE EXPLORATORIA DE DADOS ####


# Analise da duraçao em meses do credito
estatisticasDuracao <- summary(dadosCredito$credit.duration.months)
amplitudeDuracao <- (max(dadosCredito$credit.duration.months) - 
                min(dadosCredito$credit.duration.months))
desvioPadraoDuracao <- sd(dadosCredito$credit.duration.months)
duracaoMinima <- min(dadosCredito$credit.duration.months)
duracaoMaxima <- max(dadosCredito$credit.duration.months)
duracaoMedia <- mean(dadosCredito$credit.duration.months)

estatisticasDuracao 
amplitudeDuracao
desvioPadraoDuracao
duracaoMaxima
duracaoMinima
duracaoMedia

# grafico de barras da variavel duraçao
?ggplot
ggplot(dadosCredito, aes(x = dadosCredito$credit.duration.months)) + 
  geom_bar(colour = "red") + 
  xlab("Duraçao do Credito em Meses") +
  ylab("Contagem")

# boxplot da variavel duracao 
boxplot(dadosCredito$credit.duration.months, main = "Duração do credito em Meses")



# Analise da Quantia de credito solicitado
estatisticasQuantCredito <- summary(dadosCredito$credit.amount)
amplitudeQuantCredito <- (max(dadosCredito$credit.amount) - 
                                 min(dadosCredito$credit.amount))
desvioPadraoQuantCredito <- sd(dadosCredito$credit.amount)
quantidadeMinimaDeCredito <- min(dadosCredito$credit.amount)
quantidadeMaximaDeCredito <- max(dadosCredito$credit.amount)
quantidadeMediaDeCredito <- mean(dadosCredito$credit.amount)

estatisticasQuantCredito
amplitudeQuantCredito
desvioPadraoQuantCredito
quantidadeMinimaDeCredito
quantidadeMaximaDeCredito
quantidadeMediaDeCredito

# Histograma da variavel quantidade de credito
ggplot(dadosCredito, aes(x = dadosCredito$credit.amount)) +
  geom_histogram(colour = "black", binwidth = 1000) +
  xlab("Quantidade de credito solicitado") +
  ylab("Frequência")

# Box plot da variavel quantidade de credito
boxplot(dadosCredito$credit.amount, main = "BoxPlot Quantidade de Crédito Solicitado")



# Analise geral da Idade  dos requerintes
estatisticasIdade <- summary(dadosCredito$age)
amplitudeIdade <- (max(dadosCredito$age) - 
                            min(dadosCredito$age))
desvioPadraoIdade <- sd(dadosCredito$age)
idadeMinima <- min(dadosCredito$age)
idadeMaxima <- max(dadosCredito$age)
idadeMedia<- mean(dadosCredito$age)

estatisticasIdade
amplitudeIdade
desvioPadraoIdade
idadeMinima
idadeMedia
idadeMaxima


# BarPlot da varivel Idade
ggplot(dadosCredito, aes(x = dadosCredito$age)) +
  geom_bar(colour = "red") +
  xlab("Idade do Solicitante") +
  ylab("Frequencia")

# Boxplot da variavel Idade
boxplot(dadosCredito$age, main = "BoxPlot da Variavel Idade")



# CREDITO SOLICITADO VS IDADE
ggplot(dadosCredito, aes(x = dadosCredito$age, y = dadosCredito$credit.amount)) +
  geom_point(shape = 1, aes(color = age)) +
  xlab("Idade") +
  ylab("Quantia de Credito") + 
  geom_smooth(method = "lm", color = "red")



# CREDITO SOLICITADO VS DURAÇAO DO CREDITO
ggplot(dadosCredito, aes(x = dadosCredito$credit.amount, y = dadosCredito$credit.duration.months)) +
  geom_point(shape = 1, aes(color = credit.duration.months)) + 
  xlab("Quantia de Credito") +
  ylab("Duração do Credito (em meses)") +
  geom_smooth(method = "lm", color = "red")




# Normalizando as variavies numericas
variaveisNumericas <- c("age", "credit.amount", "credit.duration.months")

scale.features <- function(df, variaveis) {
  for(variavel in variaveis) {
    df[[variavel]] <- scale(df[[variavel]], center = T, scale = T)
  }
  return(df)
}


# Normalizando os dados
dadosCredito <- scale.features(dadosCredito, variaveisNumericas)
View(dadosCredito)


####### DIVIDINDO EM DADOS DE TREINO E DADOS DE TESTE #######
split <- function(dataFrame, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dadosCredito)
  trainIndex <- sample(index, trunc(length(index) * 0.7))
  dadosTreino <- dataFrame[trainIndex, ]
  dadosTeste <- dataFrame[-trainIndex, ]
  list(trainSet = dadosTreino, testSet = dadosTeste)
}

# Gerando dados de treino e de teste
splits <- split(dadosCredito)

dadosTreino <- splits$trainSet
dadosTeste <-  splits$testSet  

View(splits)



####### FEATURE SELECETION COM RANDOM FOREST #######
library(randomForest)

featureSelection_rf <- randomForest( credit.rating ~.,
                                     data = dadosCredito,
                                     ntree = 100,
                                     nodesize = 10,
                                     importance = T)
varImpPlot(featureSelection_rf)


# A partir da analise de importancia de variaveis foi possivel concluir
# que as variavies mais relevantes em primeiro momento sao:
# account.balance, credit.duration.months, previous.credit.payment.status
# credit.amount, savings, age


#### CRIANDO OS MODELOS DE CLASSIFICAÇAO ####

# Criando o modelo de random forest
modeloRandomForest_v1 <- randomForest( credit.rating ~ .
                                       - residence.duration
                                       - dependents
                                       - installment.rate
                                       - foreign.worker
                                       - telephone
                                       - employment.duration
                                       - marital.status
                                       - apartment.type,
                                       data = dadosTreino,
                                       ntree = 100,
                                       nodesize = 10)

# Imprimindo o resultado
print(modeloRandomForest_v1)

# Foi possivel constar um error rate de 23.86 e uma accuracia de 76,14% de precisao
accuracia <- 100 - 23.86
accuracia



#### GERANDO OS SCORES ####

# Fazendo previsoes
previsoes <- data.frame(observado = dadosTeste$credit.rating,
                        previsto = predict(modeloRandomForest_v1, newdata = dadosTeste)
)

# Visualizando as previsoes
View(previsoes)


### GERANDO CURVA ROC PARA AVALIAÇAO DO MODELO ###
library(ROCR)

# gerando as classes de dados
class1 <- predict(modeloRandomForest_v1, newdata = dadosTeste, type = "prob")
class2 <- dadosTeste$credit.rating

# Gerando a curva ROC
prediction <- prediction(class1[, 2], class2)
performance <- performance(prediction, "tpr", "fpr")
plot(performance, col = rainbow(10))
