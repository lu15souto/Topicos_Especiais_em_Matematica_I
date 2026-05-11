# Regreção Linear Simples entre o preço do Bitcoin e do Ouro

# Luis Guilherme Souto Miranda
# Luís Fernandes Saucedo Souza

install.packages("quantmod")  # Instalando pacote
install.packages("ggplot2") # Instalando pacote
install.packages("qqplotr") # Instalando pacote
install.packages("car") # Instalando pacote

library(quantmod) # Carregando pacote
library(ggplot2)  # Carregando pacote
library(qqplotr)  # Carregando pacote
library(car)  # Carregando pacote

ticket_1 <- "BTC-USD"
ticket_2 <- "GLD"

getSymbols(ticket_1, src = "yahoo", from = "2020-01-01", to = Sys.Date())  # Baixando os dados do ticket_1
getSymbols(ticket_2, src = "yahoo", from = "2020-01-01", to = Sys.Date())  # Baixando os dados do ticket_2

summary(`BTC-USD`) # Visualizando um resumo dos dados do ticket_1
summary(`GLD`)  # Visualizando um resumo dos dados do ticket_2

chartSeries(`BTC-USD`) # Visualizar o plot dos dados do ticket_1
chartSeries(`GLD`)  # Visualizar o plot dos dados do ticket_2

ticket_1 <- Cl(`BTC-USD`)  # Extraindo o preço de fechamento do Ibovespa e armazenando no ibov
ticket_2 <- Cl(`GLD`)  # Extraindo o preço de fechamento do Dólar e armazenando no dolar

dados <- na.omit(merge(ticket_1, ticket_2))  # Juntando ibov e dolar em uma única tabela chamada dados

colnames(dados) <- c("ticket_1", "ticket_2") # Renomeando as colunas da tabela dados

dados_df <- data.frame(Date = index(dados), coredata(dados)) # Regularizando a tabela dados para poder ser trabalhada

plot(dados_df$ticket_2, dados_df$ticket_1) # Plot do gráfico do comparativo dos dados

abline(lm(dados_df$ticket_1 ~ dados_df$ticket_2), col = "blue", lwd = 2) # Traçando a reta da regreção linear nos dados

analise <- lm(formula = ticket_1 ~ ticket_2, data = dados_df)  # Analisar oa dados da regreção linear dos dados

summary(analise)  # Visualizando um resumo dos dados da analise de regreção linear

#----------------------------------------
# Resíduos são as diferenças entre os valores reais (observados) dos dados e os valores previstos pelo modelo de regressão
#----------------------------------------

plot(analise$model$ticket_2, analise$residuals)  # Plot do gráfico do comparativo dos dados, Dolar x residuals

abline(lm(analise$residuals ~ dados_df$ticket_2), col = "blue", lwd = 2) # Traçando a reta da regreção linear nos dados, Dolar x residuals

#----------------------------------------
# Para facilitar a visualização em relação à dispersão dos resíduos e para efeito de comparação entre ajustes de modelos em que as variáveis resposta têm unidades de medida diferentes, convém padronizá-los, i.e., dividi-los pelo respectivo desvio padrão para que tenham variância igual a 1.

# Os resíduos padronizados são adimensionais e têm variância igual a 1, independentemente da variância da variável resposta. Além disso, para erros com distribuição Normal, cerca de 99% dos resíduos padronizados têm valor entre -3 e +3.
#----------------------------------------


residuos_padronizados <- rstandard(analise) # Extraindo os residuos padronizados

plot(residuos_padronizados) # Plotando os residuos padronizados

abline(lm(analise$residuals ~ dados_df$ticket_2), col = "blue", lwd = 2) # Traçando a reta da regreção linear nos dados, Dolar x residuals, no plot dos residuos padronizados

#-----------------------------------
# A distância de Cook é uma maneira de identificar pontos influentes (outliers) em um conjunto de preditores, que afetam o modelo. É uma combinação da alavancagem de cada observação e dos resíduos. Quanto maior a alavancagem, maior é a distância de Cook.
#-----------------------------------

distancia_de_cook <- cooks.distance(analise)  # Extraindo a distância de cook

plot(distancia_de_cook, type = "h", main = "Distância de Cook", ylab = "Distância de Cook", xlab = "Número da Observação") # Plotando o gráfico da distância de cook

plot(analise) # Plotando os gráficos que podem ser gerados

residuos <- residuals(analise) # Extraindo os resituos da analise de regreção linear

#------------------------------------
# Nos casos em que se supõe que os erros têm distribuição Normal, pode-se utilizar gráficos QQ (quantis-quantis) com o objetivo de avaliar se os dados são compatíveis com essa suposição. É importante lembrar que esses gráficos QQ devem ser construídos com os quantis amostrais baseados nos resíduos e não com as observações da variável resposta, pois apesar de suas distribuições também serem normais, suas médias variam com os valores associados da variável explicativa, ou seja, a média da variável resposta correspondente a yi ∈ α + βxi .

# O qq-plot indica concordância com a hipótese de Normalidade dos erros aleatórios se os pontos estiverem aproximadamente em linha reta.
#------------------------------------

qqnorm(residuos) # Plotando o QQ, para uma distribuição normal

qqline(residuos, col = "blue") # Plotando o QQ, com a regreção linear

#ou

qqnorm(residuos, pch = 20, main = "QQ-plot: residuos") # Plotando o QQ, para uma distribuição normal

qqline(residuos, col = "blue", lwd = 2) # Plotando o QQ, com a regreção linear

#ou

residuos_df <- data.frame(residuos) # Gerando uma tabela para poder ser trabalhado os residuos

ggplot(data = residuos_df, mapping = aes(sample = residuos)) + stat_qq_band(distribution = "norm", alpha = 0.3) + stat_qq_line(distribution = "norm", color = "blue") + stat_qq_point(distribution = "norm") + labs(title = "QQ-plot com bandas de confiança")

residuos <- residuals(analise) # Extraindo os resituos da analise de regreção linear

acf(residuos, main = "Autocorrelação dos resítuos")  # Autocorrelação

residuos_df <- data.frame(acf = acf(residuos, plot = FALSE)$acf, lag = acf(residuos, plot = FALSE)$lag) # Gerando uma tabela para poder ser trabalhado os residuos

ggplot(residuos_df, aes(x = lag, y = acf)) + geom_bar(stat = "identity", fill = "steelblue") + geom_hline(yintercept = 0) + geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "blue") + labs(title = "Função de Autocorrelação dos Resíduos", x = "Defasagem", y = "Autocorrelação") # Plotando a autocorrelação dos residuos

#-------------------------------------
# Durbin and Watson (1950), Durbin and Watson (1951) e Durbin and Watson (1971) produziram tabelas da distribuição da estatística D que podem ser utilizados para avaliar a suposição de que os erros são não correlacionados.

# O valor da estatística de Durbin-Watson para os dados p < 0, 005 sugere um alto grau de autocorrelação dos resíduos.
#-------------------------------------

durbinWatsonTest(analise) # Teste durbin-watson

