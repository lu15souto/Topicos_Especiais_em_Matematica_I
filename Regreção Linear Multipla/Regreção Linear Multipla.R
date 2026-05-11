# Regreção Linear Multipla para modelar o retono mensal do Ibovespa (ou outro ticket), usando como dados de comparação a variação do dólar, taxa Selic e a inflação (IPCA)

# Luis Guilherme Souto Miranda
# Luís Fernandes Saucedo Souza

install.packages("quantmod")  # Instalando pacote
install.packages("ggplot2") # Instalando pacote
install.packages("qqplotr") # Instalando pacote
install.packages("car") # Instalando pacote
install.packages("dplyr") # Instalando pacote
install.packages("lubridate") # Instalando pacote
install.packages("GetBCBData") # Instalando pacote
install.packages("zoo") # Instalando pacote
install.packages("MASS") # Instalando pacote

library(quantmod) # Carregando pacote
library(ggplot2)  # Carregando pacote
library(qqplotr)  # Carregando pacote
library(car)  # Carregando pacote
library(dplyr)  # Carregando pacote
library(lubridate)  # Carregando pacote
library(GetBCBData)  # Carregando pacote
library(zoo)  # Carregando pacote
library(MASS) # Carregando pacote

ticket <- "^BVSP"
inicio <- as.Date("2020-01-01")
fim <- Sys.Date()

getSymbols(ticket, src = "yahoo", from = inicio, to = fim)  # Baixando os dados do ticket
getSymbols("BRL=X", src = "yahoo", from = inicio, to = fim)  # Baixando os dados do dólar

ticket_mes <- to.monthly(`BVSP`, indexAt = "lastof", OHLC = FALSE) # Fechamento mensal do ticket
dolar_mes <- to.monthly(`BRL=X`, indexAt = "lastof", OHLC = FALSE) # Fechamento mensal do dólar

retorno_ticket <- diff(log(ticket_mes)) * 100 # Retornos mensais do ticket(%)
retorno_dolar <- diff(log(dolar_mes)) * 100 # Retornos mensais do dólar(%)

dados_df <- data.frame(data = index(retorno_ticket), ret_ibov = coredata(retorno_ticket), ret_dolar = coredata(retorno_dolar)) %>% mutate(ref = as.yearmon(data)) # Criando a tabela de dados

dados_df <- dados_df %>% select(ret_ibov.BVSP.Close, ret_dolar.BRL.X.Close, ref) # Selecionando as colunas desejadas

dados_df <- dados_df %>% rename(ticket = ret_ibov.BVSP.Close, dolar = ret_dolar.BRL.X.Close, data = ref) # Renomeando as colunas

series <- c(432, 433)  # 432 = Selic, 433 = IPCA

bcb <- gbcbd_get_series(id = series, first.date = inicio, last.date = fim) # Baixando os dados da Selic e IPCA

bcb <- bcb %>% mutate(data = as.yearmon(ref.date))  # Crindo a coluna data 

selic <- bcb %>% filter(id.num == 432) %>% select(ref.date, selic = value, data) # Formatando os dados da Selic
ipca  <- bcb %>% filter(id.num == 433) %>% select(ref.date, ipca = value, data) # Formatando os dados do IPCA

selic_mensal <- selic %>% mutate(fator_dia = (1 + selic / 100)^(1 / 252)) %>%  # Taxa diária equivalente (base 252 dias úteis)
  group_by(ref = as.yearmon(ref.date)) %>%    # Agrupar por mês
  summarise(fator_mensal = prod(fator_dia),  # produto acumulado dos fatores diários do mês → rentabilidade composta
  selic_mensal = (fator_mensal - 1) * 100) # converter em % de rentabilidade efetiva no mês

dados_df <- dados_df %>% left_join(selic_mensal %>% select(ref, selic_mensal), by = c("data" = "ref")) # Acrescentando a coluna selic_mensal na tabela dados

dados_df <- dados_df %>% left_join(ipca %>% select(data, ipca), by = c("data" = "data"))  # Acrescentando a coluna ipca na tabela dados

dados_df <- dados_df %>% select(data, ticket, dolar, selic_mensal, ipca)  # Organizando as colunas

rownames(dados_df) <- dados_df$data  # Organizando as colunas

dados_df$data <- NULL # Organizando as colunas

head(dados_df)

str(dados_df)

plot(dados_df$dolar, dados_df$ticket, main = "Dólar VS Ticket", xlab = "Dólar (%)", ylab = "Ticket (%)")

plot(dados_df$selic_mensal, dados_df$ticket, main = "Selic mensal VS Ticket", xlab = "Selic mensal (%)", ylab = "Ticket (%)")

plot(dados_df$ipca, dados_df$ticket, main = "IPCA VS Ticket", xlab = "IPCA (%)", ylab = "Ticket (%)")

modelo_completo <- lm(dados_df$ticket ~ dados_df$dolar + dados_df$selic_mensal + dados_df$ipca)  # Modelo com todas as variáveis

modelo_completo

summary(modelo_completo)  # Resumo detalhado do modelo

round(summary(modelo_completo)$r.squared, 4) # R-quadrado múltiplo

round(summary(modelo_completo)$adj.r.squared, 4) # R-quadrado ajustado

round(summary(modelo_completo)$sigma, 4)  # Erro padrão residual

# Testes t para cada coeficiente
coef_summary <- summary(modelo_completo)$coefficients
coef_summary

# Estatística F e valor-p
f_stat <- summary(modelo_completo)$fstatistic
f_stat

confint(modelo_completo, level = 0.95)  # Intervalos de confiança de 95% para os coeficientes

residuos <- residuals(modelo_completo)

# Histograma de resíduos
hist(residuos, main = "Distribuição dos Resíduos", 
     xlab = "Resíduos", col = "lightblue", border = "black")

# Q-Q Plot
qqnorm(residuos, main = "Q-Q Plot dos Resíduos")
qqline(residuos, col = "red")

# Resíduos vs Valores Ajustados
plot(fitted(modelo_completo), residuos, main = "Resíduos vs Valores Ajustados", xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")

#Análise de Resíduos Completa
par(mfrow = c(2, 3))
plot(modelo_completo, which = 1:6)

par(mfrow = c(1, 1))

# Fatores de Inflação da Varância (VIF)
vif(modelo_completo)

# VIF < 5: Multicolinearidade baixa/aceitável
# VIF entre 5 e 10: Multicolinearidade moderada/preocupante
# VIF > 10: Multicolinearidade severa

# y_real é a variável dependente usada no modelo
y_real <- modelo_completo$model[[1]]  # pega a primeira coluna do modelo, que é ticket

# y_previsto são os valores ajustados
y_previsto <- fitted(modelo_completo)

# Calcular métricas
metricas <- data.frame(
  RMSE = sqrt(mean((y_real - y_previsto)^2)),
  MAE  = mean(abs(y_real - y_previsto)),
  R2   = summary(modelo_completo)$r.squared
)

metricas

coef_final <- coef(modelo_completo)

coef_final

cat("Ticket_previsto =",
    round(coef_final[1], 3), "+",
    round(coef_final[2], 3), "* dolar +",
    round(coef_final[3], 3), "* selic_mensal +",
    round(coef_final[4], 3), "* ipca")

