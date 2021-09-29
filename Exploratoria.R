setwd("D:/DSA/Carreira_Cientista_Dados/MentoriaDSA/Scripts")

#### Carregando os dados do arquivo CSV (downstream) ####
arquivo.raiz <- "D:/DSA/Carreira_Cientista_Dados/MentoriaDSA"
arquivo.dataset <- paste0(arquivo.raiz, "/DataSets/dfPrecoMensal_AnoMes.csv")
arquivo.dataset2 <- paste0(arquivo.raiz, "/DataSets/dfPrecoMensal_AnoMesUSD.csv")
arquivo.imagens <- paste0(arquivo.raiz, "/Images/")


library(readr)
library(tibble)


?read_csv
# col_types = "cfffffffffffd" indica de que forma os atributos serão carregados:
#       c - character
#       f - factor
#       d - double
dfPrecoMensal <- read_csv(arquivo.dataset,
                          col_types = "cfffffffffffd")

# Tipo de dados
library(forcats)  # para usar as funções fct_count
str(dfPrecoMensal)

# Tabelas de freqência por categoria
fct_count(dfPrecoMensal$contratacao)
fct_count(dfPrecoMensal$segmentacao)
fct_count(dfPrecoMensal$in_odonto)
fct_count(dfPrecoMensal$in_obstetricia)
fct_count(dfPrecoMensal$tipo)
fct_count(dfPrecoMensal$abrangencia)
fct_count(dfPrecoMensal$fator)
fct_count(dfPrecoMensal$acomodacao)
fct_count(dfPrecoMensal$internacao)
fct_count(dfPrecoMensal$cd_faixa_etaria)
fct_count(dfPrecoMensal$nm_regiao)


str(dfPrecoMensal$contratacao)
fct_count(dfPrecoMensal$contratacao, sort = T)
# 1 Coletivo por adesão    1017114
# 2 Individual ou familiar  565368

# Extraindo uma variável da tibble
library(magrittr)
contratacao <- use_series(dfPrecoMensal, contratacao)
fct_count(contratacao, sort = T)
# 1 Coletivo por adesão    1017114
# 2 Individual ou familiar  565368
class(contratacao)
levels(contratacao)

# Valor médio de mensalidade por tipo de contratação
library(dplyr)
contratacao_summary <-
        dfPrecoMensal %>%
        group_by(ano_mes, contratacao) %>%
        summarise_at(vars(mensalidade), funs(mean))
        

#### Plotando um gráfico por atributo categórico no tempo ####
library(ggplot2)
library(plotly)  # para gráficos interativos com usuário
library(rlang)   # para obter o nome da coluna do data frame usando !!sym(category)
?sym

plot_category <- function(category, save_local=''){
        dfPrecoMensal %>%
                group_by(ano_mes, !!sym(category)) %>%
                summarise(valor = mean(mensalidade)) %>%
                ggplot(mapping=aes(x=ano_mes, y=valor,
                                   group= !!sym(category), color= !!sym(category))) +
                geom_line() +
                geom_smooth(method = lm, color = 'yellow', se = T) +
                ggtitle("Evolução dos preços dos Planos de Saúde") +
                xlab("Período") +
                ylab("Mensalidade (R$)") +
                theme(legend.position = 'top',
                      axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5, size=8),
                      axis.ticks.x = element_blank())
        if (save_local != ''){
                ggsave(paste0(save_local, 'Precos_', category, '.png'))
        }
}

# Gerando e salvando todos gráficos categóricos no tempo com valor mensal médio
colNames <- names(dfPrecoMensal)
for (c in colNames) {
        if (c != "mensalidade" && c != "ano_mes"){
                plot_category(c, save_local = arquivo.imagens)
        }
}


# Distribuição da variável alvo
summary(dfPrecoMensal$mensalidade)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.9   183.4   311.7   454.4   547.0 13739.6

boxplot(dfPrecoMensal$mensalidade,
        horizontal = TRUE,
        col = "#990000",
        main = "BoxPlot para Mensalidades")


#### Conversão das moedas em Dolar ####
# Obtendo aos meses/anos de interesse
ano_mes <- sort(unique(dfPrecoMensal$ano_mes))
ano_mes

# Get quantmod
# Warning message:
#         In doTryCatch(return(expr), name, parentenv, handler) :
#         Oanda only provides historical data for the past 180 days. Symbol: USD/BRL
# if (!require("quantmod")) {
#         install.packages("quantmod")
#         library(quantmod)
# }
# 
# getSymbols.oanda(Symbols = "USD/BRL",
#                 env = parent.frame(),
#                 return.class = "xts",
#                 index.class = 'Date',
#                 warnings = TRUE,
#                 symbol.lookup = TRUE,
#                 auto.assign = TRUE,
#                 from = '2015-08-01',
#                 to = '2015-08-31')

# Baixei as cotações no Google Fynance -  arquivo csv pois a bibioteca quantmod
# só permite acesso aos últimos 180 dias
USD_BRL <- read_csv(paste0(arquivo.raiz, '/DataSets/USD_BRL.csv'),  
                    col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                     Close = col_double(),
                                     Open = col_skip(), High = col_skip(), 
                                     Low = col_skip(), `Adj Close` = col_skip(), 
                                     Volume = col_skip()))
View(USD_BRL)
str(USD_BRL)
count(is.na(USD_BRL$Close))

# Calculando o valor do dólar médio do mês
cotacao <- USD_BRL %>%
        filter(format(Date, "%Y-%m") %in% ano_mes) %>%
        group_by(ano_mes = format(Date, "%Y-%m")) %>%
        summarise(USD_Mean = mean(Close, na.rm = TRUE))

# Não precisou usar - Função para obter a cotação e converter em dólar
# to_usd <- function(mensalidade, mes){
#         usd = as.double(cotacao[ano_mes==mes,2])
#         x = ifelse(test = !is.na(usd), round(mensalidade/usd,2), mensalidade)
#         return(x)
# }
# # Testando ...
# to_usd(568.987, '2015-10')


# Obtendo a cotação do Dolar
dfPrecoDolar <- dfPrecoMensal %>% left_join(cotacao, by = 'ano_mes') %>% select_all()
        
# Convertendo a mensalidade em USD
dfPrecoDolar$mensalidade_USD <- round(dfPrecoDolar$mensalidade/dfPrecoDolar$USD_Mean,2)

# Obtendo apenas as colunas de interesse
dfPrecoDolar <- dfPrecoDolar %>% select(-USD_Mean, -mensalidade)

# Vamos verificar se a dolarização fez alguma diferença
plot_categoryUSD <- function(category, save_local=''){
        dfPrecoDolar %>%
                group_by(ano_mes, !!sym(category)) %>%
                summarise(valor = mean(mensalidade_USD)) %>%
                ggplot(mapping=aes(x=ano_mes, y=valor,
                                   group= !!sym(category), color= !!sym(category))) +
                geom_line() +
                geom_smooth(method = lm, color = 'yellow', se = T) +
                ggtitle("Evolução dos preços em dos Planos de Saúde") +
                xlab("Período") +
                ylab("Mensalidade (US$)") +
                theme(legend.position = 'top',
                      axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5, size=8),
                      axis.ticks.x = element_blank())
        if (save_local != ''){
                ggsave(paste0(save_local, 'PrecosUSD_', category, '.png'))
        }
}
# Gerando e salvando todos gráficos categóricos no tempo com valor mensal médio
colNames <- names(dfPrecoDolar)
for (c in colNames) {
        if (c != "mensalidade_USD" && c != "ano_mes"){
                plot_categoryUSD(c, save_local = arquivo.imagens)
        }
}

#Salvando o DataSet:
write_csv(dfPrecoDolar, arquivo.dataset2)

                                