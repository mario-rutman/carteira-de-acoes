# 1. O objetivo destes scripts é que sirvam somente para atualizações. --------------------------

# Carregando os pacotes necessários.

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(purrr)


# 2. Criando manualmente os vetores com os proventos de 2014 a 2023. -------------

ITSA4 <- c(
  0.18, 0.53, 0.50, 0.38, 0.77,
  1.19, 0.65, 0.39, 0.61, 0.58
)

BBAS3 <- c(
  1.66, 1.68, 0.74, 0.94, 1.51,
  2.55, 1.48, 2.26, 4.14, 4.54
)

EGIE3 <- c(
  1.63, 1.22, 1.97, 2.72, 4.22,
  1.62, 1.72, 1.79, 2.99, 2.90
)

BBDC3 <- c(
  1.20, 1.29, 0.36, 1.03, 1.04,
  1.85, 0.64, 0.93, 0.39, 1.54
)

SANB3 <- c(
  2.09, 0.49, 0.44, 0.70, 0.84,
  1.00, 1.34, 1.35, 1.07, 0.79
)

CXSE3 <- c(
  0, 0, 0, 0, 0,
  0, 0, 0.24, 0.65, 1.00
)

SAPR4 <- c(
  0.17, 0.17, 0.21, 0.22, 0.23,
  0.29, 0.20, 0.22, 0.31, 0.31
)

BRAP3 <- c(
  1.56, 0.94, 0.10, 1.30, 1.75,
  1.27, 1.82, 10.24, 2.97, 2.75
)

CSMG3 <- c(
  0.38, 0.03, 0.14, 0.70, 1.34,
  0.30, 2.79, 0.94, 0.38, 2.89
)

WIZC3 <- c(
  0.00, 0.00, 0.68, 0.71, 0.51,
  1.11, 0.67, 0.61, 0.59, 0.45
)

CPFE3 <- c(
  1.03, 0.00, 0.21, 0.22, 0.28,
  0.48, 1.80, 3.70, 3.24, 2.89
)


# 3. Criando o dataframe prov_2014_2023 -----------------------------------

sigla <- rep(c(
  "BBAS3", "BBDC3", "BRAP3", "CPFE3", "CSMG3", "CXSE3",
  "EGIE3", "ITSA4", "SANB3",
  "SAPR4", "WIZC3"
), each = 10)

prov <- c(
  BBAS3, BBDC3, BRAP3, CPFE3, CSMG3, CXSE3,
  EGIE3, ITSA4, SANB3,
  SAPR4, WIZC3
)

# Converter os vetores em um dataframe.
prov_2014_2023 <- data.frame(ticker = sigla, provento = prov)

# Acrescentar coluna com os nomes das empresas.
prov_2014_2023 <- left_join(prov_2014_2023, acoes_b3, by = "ticker")

# Salvando em rds.
saveRDS(prov_2014_2023, "data-raw/RDS/prov_2014_2023.rds")

# 4. Criando o df preco_atual. --------------------------------------------

preco_atual <- read_csv("data-raw/excel/carteira.csv",
  col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))
) |>
  group_by(symbol) |>
  filter(
    timestamp == max(timestamp),
    close <= 1000
  ) |>
  ungroup() |>
  select(
    preco_atual = close,
    ticker = symbol
  )

saveRDS(preco_atual, "data-raw/RDS/preco_atual.rds")


# 5. Calculando o preço teto para cada margem de segurança. ---------------

# Fala-se em um preço teto de uma ação pressupondo um DY de 6%.
# Isto é, até que preço posso pagar por uma ação para ter uma rentalidade de 6%,
# considerando o que espero receber de proventos.
# Se espero ou acredito que receberei R$3,50 de dividentos/proventos em um ano,
# O máximo que devo pagar por uma ação é R$58,33.

# Como é incerto o valor do dividendo trabalha-se com margens de segurança:

# A margem de segurança alta é calculada pelo valor do 2º decil dos proventos.
# A margem de segurança media é calculada pelo valor do 3,5º decil dos proventos.
# A margem de segurança baixa é calculada pelo valor da mediana (5º decil) dos proventos.


prov_faixas <- prov_2014_2023 |>
  group_by(nome, ticker) |>
  summarise(
    baixa = round((quantile(provento, probs = 0.50, na.rm = TRUE)), 2),
    media = round((quantile(provento, probs = 0.35, na.rm = TRUE)), 2),
    alta = round((quantile(provento, probs = 0.20, na.rm = TRUE)), 2)
  ) |>
  pivot_longer(
    cols = c(baixa, media, alta),
    names_to = "marg_segur",
    values_to = "hipotese_de_proventos"
  ) |>
  left_join(preco_atual, by = "ticker") |>
  mutate(DY_se_hipot_se_confirmar = round(hipotese_de_proventos * 100 / preco_atual, 1))

# Salvando em rds.
saveRDS(prov_faixas, "data-raw/RDS/prov_faixas.rds")


# 6. Juntando o df prov_faixas com df carteira.  -----------------------

df_completo <- read_csv("data-raw/excel/carteira.csv",
  col_types = cols(timestamp = col_date(format = "%Y-%m-%d"))
) |>
  # renomeando e selecionando
  select(
    ticker = symbol,
    data = timestamp,
    preco_acao = close
  ) |>
  filter(preco_acao <= 1000) |>
  distinct() |>
  # Juntando com a tabela prov_faixas.
  left_join(prov_faixas, by = "ticker") |>
  unite(ticker, ticker, nome, sep = " - ") |> 
  mutate(preco_teto_barsi = round(hipotese_de_proventos / 0.06, 2))

# Salvando em rds.
saveRDS(df_completo, "data-raw/RDS/df_completo.rds")

# 7. Criando a função para fazer um gráfico. ------------------------------

analise_preco_baixo <- function(sigla_acao) {
  ultimo_preco <- df_completo |>
    filter(
      ticker == sigla_acao,
      data == max(data)
    ) |>
    pull(preco_acao) |>
    unique()

  faixa_segur <- df_completo |>
    filter(ticker == sigla_acao) |>
    filter(data == max(data, na.rm = TRUE)) |>
    pull(preco_teto_barsi)
  
  titulo <- df_completo |>
    filter(ticker == sigla_acao) |>
    select(DY_se_hipot_se_confirmar) |> 
    distinct() |> 
    pull()
    

  # Fazendo o gráfico.
  grafico <- df_completo |>
    filter(ticker == sigla_acao) |>
    ggplot(aes(x = ticker, y = preco_acao)) +
    geom_boxplot(width = 0.5, notch = FALSE) +
    geom_hline(aes(yintercept = ultimo_preco), color = "red2", linewidth = 1.4) +
    # geom_text(aes(y = -1, label = paste(DY_se_hipot_se_confirmar[1],"-")),
    #   x = 0.5, hjust = 0, vjust = 0, color = "black") +
    # geom_text(aes(y = -1, label = paste(DY_se_hipot_se_confirmar[2],"-")),
    #   x = 0.67, hjust = 0, vjust = 0, color = "black") +
    # geom_text(aes(y = -1, label = paste(DY_se_hipot_se_confirmar[3],"-")),
    #   x = 0.79, hjust = 0, vjust = 0, color = "black") +
    ggtitle(paste(titulo[1],"%: ",titulo[2],"%: ",titulo[3],"%: ")) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15))

  return(grafico)
}


# Exemplo passando todas as ações pela função.

todas_acoes <- df_completo$ticker |>
  unique() |>
  sort()

# graficos é uma lista com todos os gráficos.
graficos <- map(todas_acoes, analise_preco_baixo)

# Exemplos.
graficos[10]
graficos[9]


# 8. Reunindo todos pelo patchwork. ---------------------------------------------------------------------


# Organizar os gráficos em um layout de 6 colunas

layout <- wrap_plots(graficos[1:length(graficos)], ncol = 4)
layout
