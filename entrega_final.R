# ENTREGA FINAL: ECONOLAB 
# GRUPO: AMANDA RIBEIRO, GUSTAVO MENDES, GUILHERME ROCCATO, LU?S FELIPE YAMASHITAFUJI E PEDRO DUTRA


# REPLICANDO A METODOLOGIA DO PAPER

# PER?ODO: 2004 - 2023

install.packages("lmtest")
install.packages("sandwich")

# Carregar pacotes
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(lmtest)
library(sandwich)
library(ggplot2)

# Ler cada planilha

df_salario_minimo <- read_excel("salario_minimo.xlsx") #dado mensal

df_hiato_produto <- read_excel("hiato_produto.xlsx", skip = 8)
 
df_ipca_servicos <- read_excel("ipca_servicos.xlsx") #dado mensal 

# df_2000_2010 <- read_excel("expectativa_inflacao_anual_2000_2010.xls", skip = 1) #dado anual 
# df_2011_2021 <- read_excel("expectativa_inflacao_anual_2011_2021.xls", skip = 1) #dado anual
# df_2022_2025 <- read_excel("expectativa_inflacao_anual_2022_2025.xls", skip = 1) #dado anual 
# 
# # Verifique os nomes das colunas e padronize, se necess?rio
# names(df_2000_2010)
# names(df_2011_2021)
# names(df_2022_2025)
# 
# Unir os tr?s dataframes
# df_unido <- bind_rows(df_2000_2010, df_2011_2021, df_2022_2025)
# 
# # resumo da estrutura dos nosso dados
# # glimpse(df_unido)

# DATAFRAME DE REAJUSTE ANUAL DO SALARIO MINIMO
df_reajuste_anual <- df_salario_minimo %>%
  # Renomear colunas
  rename(data = 1, salario = 2) %>%
  # Converter salário para numérico
  mutate(salario = as.numeric(salario)) %>%
  # Calcular variação para todos os registros
  mutate(variacao = c(NA, salario[2:n()] / salario[1:(n()-1)] * 100 - 100)) %>%
  # Arredondar para duas casas decimais
  mutate(variacao = round(variacao, 2)) %>%
  # Filtrar anos entre 2004 e 2023
  filter(year(data) >= 2004 & year(data) <= 2023) %>%
  # Filtrar apenas onde há variação
  filter(!is.na(variacao) & variacao != 0) %>%
  # Agrupar por ano e pegar o primeiro mês com variação
  group_by(year(data)) %>%
  slice_min(order_by = month(data), n = 1) %>%
  ungroup() %>%
  # Selecionar colunas finais
  select(data, salario, variacao)

#tail(df_reajuste_anual)


# DATAFRAME DE INFLACAO DE SERVICOS ACUMULADA NOS ULTIMOS 12 MESES
# Preparar df_ipca_servicos2: converter datas e valores para formatos adequados
df_ipca_servicos2 <- df_ipca_servicos %>%
  # Renomear colunas para nomes mais simples
  rename(Data = 1, ipca = 2) %>%
  # Filtrar apenas linhas com Data contendo números inteiros (números seriais Excel)
  filter(grepl("^[0-9]+$", Data)) %>%
  # Converter Data para numérico (serial Excel)
  mutate(Data = as.numeric(Data)) %>%
  # Converter número serial Excel para objeto Date do R
  mutate(Data = as.Date(Data, origin = "1899-12-30")) %>%
  # Converter ipca para numérico
  mutate(ipca = as.numeric(ipca))

#head(df_ipca_servicos2)

# Calcular IPCA acumulado nos 12 meses anteriores para cada data de reajuste
df_servicos12m_anteriores <- df_reajuste_anual %>%
  mutate(
    servicos12m = map_dbl(data, function(d) {
      # Definir janela: do mês 12 meses antes até o mês imediatamente anterior a d
      inicio <- d %m-% months(12)
      fim <- d %m-% months(1)
      
      # Filtrar dados do IPCA dentro da janela temporal e calcular acumulado percentual
      df_ipca_servicos2 %>%
        filter(Data >= inicio, Data <= fim) %>%
        summarise(acum = (prod(1 + ipca / 100) - 1) * 100) %>%
        pull(acum)
    })
  ) %>%
  # Selecionar colunas finais para o output
  select(data, servicos12m) %>%
  # Arredondar resultado para duas casas decimais
  mutate(servicos12m = round(servicos12m, 2))

#tail(df_servicos12m_anteriores)


# DATAFRAME DE INFLACAO DE SERVICOS ACUMULADA NOS 12 MESES FUTUROS
df_servicos12m_posteriores <- df_reajuste_anual %>%
  mutate(
    servicos12m_adiante = map_dbl(data, function(d) {
      # Janela: de d até (d + 11 meses) — 12 meses no total
      inicio <- d
      fim <- d %m+% months(11)
      
      # Filtrar dados do IPCA dentro da janela temporal e calcular acumulado percentual
      df_ipca_servicos2 %>%
        filter(Data >= inicio, Data <= fim) %>%
        summarise(acum = (prod(1 + ipca / 100) - 1) * 100) %>%
        pull(acum)
    })
  ) %>%
  # Selecionar colunas finais
  select(data, servicos12m_adiante) %>%
  # Arredondar resultado para duas casas decimais
  mutate(servicos12m_adiante = round(servicos12m_adiante, 2))

#tail(df_servicos12m_posteriores)


# DATAFRAME EXPECTATIVAS DE INFLACAO
valores_expectativa <- c(
  5.52, 5.69, 4.40, 3.84, 4.44, 4.91, 4.36, 5.60,
  5.45, 5.42, 6.16, 6.60, 7.11, 4.79, 4.04, 3.34,
  4.20, 4.12, 5.91, 5.45
)

df_expectativas <- df_reajuste_anual %>%
  arrange(data) %>%
  mutate(expectativa = valores_expectativa) %>%
  select(data, expectativa)

#tail(df_expectativas)


# DATAFRAME DE HIATO DO PRODUTO
# Preparar df_hiato_produto: renomear colunas e converter datas
df_hiato_produto2 <- df_hiato_produto %>%
  rename(data_hiato = Trimestre, hiato = `Hiato`) %>%
  mutate(data_hiato = as.Date(data_hiato))

# Combinar datas de reajuste com datas de hiato
df_hiato <- df_reajuste_anual %>%
  select(data) %>%
  mutate(key = 1) %>%
  inner_join(
    df_hiato_produto2 %>% mutate(key = 1),
    by = "key",
    relationship = "many-to-many"
  ) %>%
  select(-key) %>%
  # Filtrar apenas datas anteriores
  filter(data_hiato < data) %>%
  # Para cada data, pegar o hiato mais recente anterior
  group_by(data) %>%
  filter(data_hiato == max(data_hiato)) %>%
  ungroup() %>%
  select(data, hiato)

#tail(df_hiato)


# DATAFRAME ESPELHO DO RELATORIO DO BANCO CENTRAL
df_relatorio_bc <- df_reajuste_anual %>%
  left_join(df_servicos12m_anteriores, by = "data") %>%
  left_join(df_servicos12m_posteriores, by = "data") %>%
  left_join(df_expectativas, by = "data") %>%
  left_join(df_hiato, by = "data")

print(df_relatorio_bc)


# ESTIMANDO MODELO
modelo <- lm(
  servicos12m_adiante ~ variacao + servicos12m + expectativa + hiato,
  data = df_relatorio_bc
  )

coeftest(modelo, vcov = NeweyWest(modelo, lag = 1, prewhite = FALSE))


# GRAFICO
# Preparar dados
df_plot <- df_relatorio_bc %>%
  mutate(ano = year(data))

# Ajuste da regressão simples
fit <- lm(servicos12m_adiante ~ variacao, data = df_plot)
coef_int   <- coef(fit)[1]
coef_slope <- coef(fit)[2]
r2         <- summary(fit)$r.squared
eq_label   <- paste0("y = ", round(coef_slope, 1),
                     " x + ", round(coef_int,   1),
                     "\nR² = ", round(r2,        2))

# Valores mínimos e máximos para posicionar o label
x_min <- min(df_plot$variacao)
x_max <- max(df_plot$variacao)
y_min <- min(df_plot$servicos12m_adiante)
y_max <- max(df_plot$servicos12m_adiante)

# Construção do gráfico
ggplot(df_plot, aes(x = variacao, y = servicos12m_adiante)) +
  
  # Pontos e rótulos
  geom_point(size = 3, color = "#2C3E50") +
  geom_text(aes(label = ano), vjust = -1.2, size = 3.5, color = "#34495E") +
  
  # Linha de regressão pontilhada
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "#E74C3C") +
  
  # Anotação da equação e R²
  annotate(
    "text",
    x    = x_min + 0.001 * (x_max - x_min),
    y    = y_max - 0.05 * (y_max - y_min),
    label= eq_label,
    hjust= 0,
    size = 4,
    color= "#E74C3C"
  ) +
  
  # Escalas com quebras principais e secundárias
  scale_x_continuous(
    breaks      = seq(floor(x_min), ceiling(x_max), by = 1),
    minor_breaks= seq(floor(x_min), ceiling(x_max), by = 1),
    expand      = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    breaks      = seq(floor(y_min), ceiling(y_max), by = 1),
    minor_breaks= seq(floor(y_min), ceiling(y_max), by = 1),
    expand      = expansion(mult = c(0.1, 0.1))
  ) +
  
  # Títulos e rótulos
  labs(
    title = "Variação do Salário Mínimo em t vs Inflação de Serviços de t a t+11 (%)",
    x     = "Variação do Salário Mínimo em t (%)",
    y     = "Inflação de serviços de t a t+11 (%)"
  ) +
  
  # Tema com grades
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    panel.grid.major       = element_line(color = "gray80", size = 0.5),
    panel.grid.minor       = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y     = element_line(linetype = "dashed"),
    panel.grid.minor.x     = element_line(linetype = "dashed"),
    panel.grid.major.x     = element_line(linetype = "solid"),
    panel.grid.major.y     = element_line(linetype = "solid"),
    axis.ticks             = element_line(color = "gray50"),
    plot.title             = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title             = element_text(face = "bold", size = 14)
  )

